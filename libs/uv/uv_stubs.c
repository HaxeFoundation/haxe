#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <stdio.h>
#include <stdlib.h>
#include <uv.h>

#if (UV_VERSION_MAJOR <= 0)
#	error "libuv1-dev required, uv version 0.x found"
#endif

// ------------- UTILITY MACROS -------------------------------------

/**
	The `data` field of handles and requests is used to store OCaml callbacks.
	These callbacks are called from the various `handle_...` functions, after
	pre-processing libuv results as necessary. At runtime, a callback is simply
	a `value`. To ensure it is not garbage-collected, we add the data pointer of
	the handle or request to OCaml's global GC roots, then remove it after the
	callback is called.
**/

// access the data of a handle or request
#define UV_HANDLE_DATA(h) (((uv_handle_t *)(h))->data)
#define UV_HANDLE_DATA_A(h) ((value *)(&UV_HANDLE_DATA(h)))
#define UV_REQ_DATA(r) (((uv_req_t *)(r))->data)
#define UV_REQ_DATA_A(r) ((value *)(&UV_REQ_DATA(r)))

// malloc a single value of the given type
#define UV_ALLOC(t) ((t *)malloc(sizeof(t)))

// unwrap an abstract block (see UV_ALLOC_CHECK notes below)
#define UV_UNWRAP(v, t) ((t *)Field(v, 0))

/**
	OCaml requires a two-method implementation for any function that takes 6 or
	more arguments. The "bytecode" part receives an array and simply forwards it
	to the "native" part (assuming no unboxed calls). These macros define the
	bytecode part for the given function.
**/

#define BC_WRAP6(name) \
	CAMLprim value name ## _bytecode(value *argv, int argc) { \
		return name(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]); \
	}
#define BC_WRAP7(name) \
	CAMLprim value name ## _bytecode(value *argv, int argc) { \
		return name(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]); \
	}

// ------------- ERROR HANDLING -------------------------------------

/**
	UV_ERROR, UV_SUCCESS_UNIT, and UV_SUCCESS take place of returns in functions
	with a `T cb_result` return type (in uv.ml). `T cb_result` is a polymorphic
	type with two variants - error of int and success of T.

	UV_ALLOC_CHECK tries to allocate a variable of the given type with the given
	name and calls UV_ERROR if this fails. UV_ALLOC_CHECK_C is the same, but
	allows specifying custom clean-up code before the error result is returned.
	Allocation returns a value that is a block with Abstract_tag, with its single
	field pointing to the malloc'ed native value.

	UV_ERROR_CHECK checks for a libuv error in the given int expression (indicated
	by a negative value), and in case of an error, calls UV_ERROR. Once again,
	UV_ERROR_CHECK_C is the same, but allows specifying custom clean-up code.

	All of these functions are only usable in OCaml-initialised functions, i.e.
	CAMLparam... and CAMLreturn... are required.
**/

#define UV_ERROR(errno) do { \
		CAMLlocal1(_res); \
		_res = caml_alloc(1, 0); \
		Field(_res, 0) = Val_int(errno); \
		CAMLreturn(_res); \
	} while (0)

#define UV_SUCCESS(success_value) do { \
		CAMLlocal1(_res); \
		_res = caml_alloc(1, 1); \
		Field(_res, 0) = (value)(success_value); \
		CAMLreturn(_res); \
	} while (0)

#define UV_SUCCESS_UNIT UV_SUCCESS(Val_unit);

#define UV_ALLOC_CHECK_C(var, type, cleanup) \
	type *_native = UV_ALLOC(type); \
	if (_native == NULL) { \
		cleanup; \
		UV_ERROR(0); \
	} \
	CAMLlocal1(var); \
	var = caml_alloc(1, Abstract_tag); \
	Store_field(var, 0, (value)_native);

#define UV_ALLOC_CHECK(var, type) UV_ALLOC_CHECK_C(var, type, )

#define UV_ERROR_CHECK_C(expr, cleanup) do { \
		int _code = expr; \
		if (_code < 0) { \
			cleanup; \
			UV_ERROR(_code); \
		} \
	} while (0)

#define UV_ERROR_CHECK(expr) UV_ERROR_CHECK_C(expr, )

// ------------- LOOP -----------------------------------------------

CAMLprim value w_loop_init(value unit) {
	CAMLparam1(unit);
	UV_ALLOC_CHECK(loop, uv_loop_t);
	UV_ERROR_CHECK_C(uv_loop_init(UV_UNWRAP(loop, uv_loop_t)), free(UV_UNWRAP(loop, uv_loop_t)));
	UV_SUCCESS(loop);
}

CAMLprim value w_loop_close(value loop) {
	CAMLparam1(loop);
	UV_ERROR_CHECK(uv_loop_close(UV_UNWRAP(loop, uv_loop_t)));
	free(UV_UNWRAP(loop, uv_loop_t));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_run(value loop, value mode) {
	CAMLparam2(loop, mode);
	UV_SUCCESS(Val_bool(uv_run(UV_UNWRAP(loop, uv_loop_t), (uv_run_mode)Int_val(mode)) == 0));
}

CAMLprim value w_loop_alive(value loop) {
	CAMLparam1(loop);
	UV_SUCCESS(Val_bool(uv_loop_alive(UV_UNWRAP(loop, uv_loop_t)) != 0));
}

CAMLprim value w_stop(value loop) {
	CAMLparam1(loop);
	uv_stop(UV_UNWRAP(loop, uv_loop_t));
	UV_SUCCESS_UNIT;
}

// ------------- FILESYSTEM -----------------------------------------

// (no-op) typecast to juggle value and uv_file (which is an unboxed integer)
#define Val_file(f) ((value)(f))
#define File_val(v) ((uv_file)(v))

/**
	FS handlers all have the same structure.

	The async version (no suffix) calls the callback with a `T cb_result` value.

	The sync version (`_sync` suffix) returns `T` directly, which will need to be
	wrapped into a `T cb_result` in the calling function.
**/

#define UV_FS_HANDLER(name, setup) \
	static void name(uv_fs_t *req) { \
		CAMLparam0(); \
		CAMLlocal2(cb, res); \
		cb = (value)UV_REQ_DATA(req); \
		res = caml_alloc(1, req->result < 0 ? 0 : 1); \
		if (req->result < 0) \
			Store_field(res, 0, req->result); \
		else { \
			CAMLlocal1(value2); \
			do setup while (0); \
			Store_field(res, 0, value2); \
		} \
		caml_callback(cb, res); \
		uv_fs_req_cleanup(req); \
		caml_remove_global_root(UV_REQ_DATA_A(req)); \
		free(req); \
		CAMLreturn0; \
	} \
	static value name ## _sync(uv_fs_t *req) { \
		CAMLparam0(); \
		CAMLlocal1(value2); \
		do setup while (0); \
		CAMLreturn(value2); \
	}

UV_FS_HANDLER(handle_fs_cb, value2 = Val_unit;);
UV_FS_HANDLER(handle_fs_cb_bytes, value2 = caml_copy_string((const char *)req->ptr););
UV_FS_HANDLER(handle_fs_cb_path, value2 = caml_copy_string((const char *)req->path););
UV_FS_HANDLER(handle_fs_cb_int, value2 = Val_int(req->result););
UV_FS_HANDLER(handle_fs_cb_file, value2 = Val_file(req->result););
UV_FS_HANDLER(handle_fs_cb_stat, {
		value2 = caml_alloc(20, 0);
		Store_field(value2, 0, Val_long(req->statbuf.st_dev));
		Store_field(value2, 1, Val_long(req->statbuf.st_mode));
		Store_field(value2, 2, Val_long(req->statbuf.st_nlink));
		Store_field(value2, 3, Val_long(req->statbuf.st_uid));
		Store_field(value2, 4, Val_long(req->statbuf.st_gid));
		Store_field(value2, 5, Val_long(req->statbuf.st_rdev));
		Store_field(value2, 6, Val_long(req->statbuf.st_ino));
		Store_field(value2, 7, caml_copy_int64(req->statbuf.st_size));
		Store_field(value2, 8, Val_long(req->statbuf.st_blksize));
		Store_field(value2, 9, Val_long(req->statbuf.st_blocks));
		Store_field(value2, 10, Val_long(req->statbuf.st_flags));
		Store_field(value2, 11, Val_long(req->statbuf.st_gen));
		Store_field(value2, 12, caml_copy_int64(req->statbuf.st_atim.tv_sec));
		Store_field(value2, 13, Val_long(req->statbuf.st_atim.tv_nsec));
		Store_field(value2, 14, caml_copy_int64(req->statbuf.st_mtim.tv_sec));
		Store_field(value2, 15, Val_long(req->statbuf.st_mtim.tv_nsec));
		Store_field(value2, 16, caml_copy_int64(req->statbuf.st_ctim.tv_sec));
		Store_field(value2, 17, Val_long(req->statbuf.st_ctim.tv_nsec));
		Store_field(value2, 18, caml_copy_int64(req->statbuf.st_birthtim.tv_sec));
		Store_field(value2, 19, Val_long(req->statbuf.st_birthtim.tv_nsec));
	});
UV_FS_HANDLER(handle_fs_cb_scandir, {
		uv_dirent_t ent;
		value2 = Val_int(0);
		while (uv_fs_scandir_next(req, &ent) != UV_EOF) {
			CAMLlocal2(dirent, node);
			dirent = caml_alloc(2, 0);
			Store_field(dirent, 0, caml_copy_string(ent.name));
			Store_field(dirent, 1, Val_int(ent.type));
			node = caml_alloc(2, 0);
			Store_field(node, 0, dirent); // car
			Store_field(node, 1, value2); // cdr
			value2 = node;
		}
	});

/**
	Most FS functions from libuv can be wrapped with FS_WRAP (or one of the
	FS_WRAP# variants defined below) - create a request, register a callback for
	it, register the callback with the GC, perform request. Then, either in the
	handler function (synchronous or asynchronous), the result is checked and
	given to the OCaml callback if successful, with the appropriate value
	conversions done, as defined in the various UV_FS_HANDLERs above.
**/

#define FS_WRAP(name, sign, locals, precall, call, handler) \
	CAMLprim value w_ ## name(value loop, sign, value cb) { \
		CAMLparam2(loop, cb); \
		locals; \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		UV_REQ_DATA(UV_UNWRAP(req, uv_fs_t)) = (void *)cb; \
		caml_register_global_root(UV_REQ_DATA_A(UV_UNWRAP(req, uv_fs_t))); \
		precall \
		UV_ERROR_CHECK_C(uv_ ## name(UV_UNWRAP(loop, uv_loop_t), UV_UNWRAP(req, uv_fs_t), call, handler), free(UV_UNWRAP(req, uv_fs_t))); \
		UV_SUCCESS_UNIT; \
	} \
	CAMLprim value w_ ## name ## _sync(value loop, sign) { \
		CAMLparam1(loop); \
		locals; \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		caml_register_global_root(UV_REQ_DATA_A(req)); \
		precall \
		UV_ERROR_CHECK_C(uv_ ## name(UV_UNWRAP(loop, uv_loop_t), UV_UNWRAP(req, uv_fs_t), call, NULL), free(UV_UNWRAP(req, uv_fs_t))); \
		UV_ERROR_CHECK_C(UV_UNWRAP(req, uv_fs_t)->result, { uv_fs_req_cleanup(UV_UNWRAP(req, uv_fs_t)); free(UV_UNWRAP(req, uv_fs_t)); }); \
		CAMLlocal1(ret); \
		ret = handler ## _sync(UV_UNWRAP(req, uv_fs_t)); \
		uv_fs_req_cleanup(UV_UNWRAP(req, uv_fs_t)); \
		free(UV_UNWRAP(req, uv_fs_t)); \
		UV_SUCCESS(ret); \
	}

#define COMMA ,
#define FS_WRAP1(name, arg1conv, handler) \
	FS_WRAP(name, value arg1, CAMLxparam1(arg1), , arg1conv(arg1), handler);
#define FS_WRAP2(name, arg1conv, arg2conv, handler) \
	FS_WRAP(name, value arg1 COMMA value arg2, CAMLxparam2(arg1, arg2), , arg1conv(arg1) COMMA arg2conv(arg2), handler);
#define FS_WRAP3(name, arg1conv, arg2conv, arg3conv, handler) \
	FS_WRAP(name, value arg1 COMMA value arg2 COMMA value arg3, CAMLxparam3(arg1, arg2, arg3), , arg1conv(arg1) COMMA arg2conv(arg2) COMMA arg3conv(arg3), handler);
#define FS_WRAP4(name, arg1conv, arg2conv, arg3conv, arg4conv, handler) \
	FS_WRAP(name, value arg1 COMMA value arg2 COMMA value arg3 COMMA value arg4, CAMLxparam4(arg1, arg2, arg3, arg4), , arg1conv(arg1) COMMA arg2conv(arg2) COMMA arg3conv(arg3) COMMA arg4conv(arg4), handler); \
	BC_WRAP6(w_ ## name);

FS_WRAP1(fs_close, File_val, handle_fs_cb);
FS_WRAP3(fs_open, String_val, Int_val, Int_val, handle_fs_cb_file);
FS_WRAP1(fs_unlink, String_val, handle_fs_cb);
FS_WRAP2(fs_mkdir, String_val, Int_val, handle_fs_cb);
FS_WRAP1(fs_mkdtemp, String_val, handle_fs_cb_path);
FS_WRAP1(fs_rmdir, String_val, handle_fs_cb);
FS_WRAP2(fs_scandir, String_val, Int_val, handle_fs_cb_scandir);
FS_WRAP1(fs_stat, String_val, handle_fs_cb_stat);
FS_WRAP1(fs_fstat, File_val, handle_fs_cb_stat);
FS_WRAP1(fs_lstat, String_val, handle_fs_cb_stat);
FS_WRAP2(fs_rename, String_val, String_val, handle_fs_cb);
FS_WRAP1(fs_fsync, File_val, handle_fs_cb);
FS_WRAP1(fs_fdatasync, File_val, handle_fs_cb);
FS_WRAP2(fs_ftruncate, File_val, Int64_val, handle_fs_cb);
FS_WRAP4(fs_sendfile, File_val, File_val, Int_val, Int_val, handle_fs_cb);
FS_WRAP2(fs_access, String_val, Int_val, handle_fs_cb);
FS_WRAP2(fs_chmod, String_val, Int_val, handle_fs_cb);
FS_WRAP2(fs_fchmod, File_val, Int_val, handle_fs_cb);
FS_WRAP3(fs_utime, String_val, Double_val, Double_val, handle_fs_cb);
FS_WRAP3(fs_futime, File_val, Double_val, Double_val, handle_fs_cb);
FS_WRAP2(fs_link, String_val, String_val, handle_fs_cb);
FS_WRAP3(fs_symlink, String_val, String_val, Int_val, handle_fs_cb);
FS_WRAP1(fs_readlink, String_val, handle_fs_cb_bytes);
FS_WRAP1(fs_realpath, String_val, handle_fs_cb_bytes);
FS_WRAP3(fs_chown, String_val, (uv_uid_t)Int_val, (uv_gid_t)Int_val, handle_fs_cb);
FS_WRAP3(fs_fchown, File_val, (uv_uid_t)Int_val, (uv_gid_t)Int_val, handle_fs_cb);

/**
	`fs_read` and `fs_write` require a tiny bit of setup just before the libuv
	request is actually started; namely, a buffer structure needs to be set up,
	which is simply a wrapper of a pointer to the OCaml bytes value.

	libuv actually supports multiple buffers in both calls, but this is not
	mirrored in the Haxe API, so only a single-buffer call is used.
**/

FS_WRAP(fs_read,
	value file COMMA value buffer COMMA value offset COMMA value length COMMA value position,
	CAMLxparam5(file, buffer, offset, length, position),
	uv_buf_t buf = uv_buf_init(&Byte(buffer, Int_val(offset)), Int_val(length));,
	File_val(file) COMMA &buf COMMA 1 COMMA Int_val(position),
	handle_fs_cb_int);
BC_WRAP7(w_fs_read);
BC_WRAP6(w_fs_read_sync);

FS_WRAP(fs_write,
	value file COMMA value buffer COMMA value offset COMMA value length COMMA value position,
	CAMLxparam5(file, buffer, offset, length, position),
	uv_buf_t buf = uv_buf_init(&Byte(buffer, Int_val(offset)), Int_val(length));,
	File_val(file) COMMA &buf COMMA 1 COMMA Int_val(position),
	handle_fs_cb_int);
BC_WRAP7(w_fs_write);
BC_WRAP6(w_fs_write_sync);

// ------------- FILESYSTEM EVENTS ----------------------------------

static void handle_fs_event_cb(uv_fs_event_t *handle, const char *filename, int events, int status) {
	CAMLparam0();
	CAMLlocal2(cb, res);
	cb = (value)UV_HANDLE_DATA(handle);
	res = caml_alloc(1, status < 0 ? 0 : 1);
	if (status < 0)
		Store_field(res, 0, status);
	else {
		CAMLlocal1(event);
		event = caml_alloc(2, 0);
		Store_field(event, 0, caml_copy_string(filename));
		Store_field(event, 1, events);
		Store_field(res, 0, event);
	}
	caml_callback(cb, res);
	CAMLreturn0;
}

CAMLprim value w_fs_event_start(value loop, value path, value persistent, value recursive, value cb) {
	CAMLparam4(loop, path, recursive, cb);
	UV_ALLOC_CHECK(handle, uv_fs_event_t);
	UV_ERROR_CHECK_C(uv_fs_event_init(UV_UNWRAP(loop, uv_loop_t), UV_UNWRAP(handle, uv_fs_event_t)), free(UV_UNWRAP(handle, uv_fs_event_t)));
	UV_HANDLE_DATA(UV_UNWRAP(handle, uv_fs_event_t)) = (void *)cb;
	caml_register_global_root(UV_HANDLE_DATA_A(UV_UNWRAP(handle, uv_fs_event_t)));
	fflush(stdout);
	UV_ERROR_CHECK_C(uv_fs_event_start(UV_UNWRAP(handle, uv_fs_event_t), handle_fs_event_cb, String_val(path), Bool_val(recursive) ? UV_FS_EVENT_RECURSIVE : 0), free(UV_UNWRAP(handle, uv_fs_event_t)));
	if (!Bool_val(persistent))
		uv_unref(UV_UNWRAP(handle, uv_handle_t));
	UV_SUCCESS(handle);
}

CAMLprim value w_fs_event_stop(value handle) {
	CAMLparam1(handle);
	UV_ERROR_CHECK_C(uv_fs_event_stop(UV_UNWRAP(handle, uv_fs_event_t)), free(UV_UNWRAP(handle, uv_fs_event_t)));
	caml_remove_global_root(UV_HANDLE_DATA_A(UV_UNWRAP(handle, uv_fs_event_t)));
	free(UV_UNWRAP(handle, uv_fs_event_t));
	UV_SUCCESS_UNIT;
}
