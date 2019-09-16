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

	Handle-specific macros are defined further, in the HANDLE DATA section.
**/

// access the data of a request
#define UV_REQ_DATA(r) (((uv_req_t *)(r))->data)
#define UV_REQ_DATA_A(r) ((value *)(&UV_REQ_DATA(r)))

// allocate a request, add its callback to GC roots
#define UV_ALLOC_REQ(name, type, cb) \
	UV_ALLOC_CHECK(name, type); \
	UV_REQ_DATA(UV_UNWRAP(name, type)) = (void *)cb; \
	caml_register_global_root(UV_REQ_DATA_A(UV_UNWRAP(name, type)));

// free a request, remove its callback from GC roots
#define UV_FREE_REQ(name) \
	caml_remove_global_root(UV_REQ_DATA_A(name)); \
	free(name);

// malloc a single value of the given type
#define UV_ALLOC(t) ((t *)malloc(sizeof(t)))

// unwrap an abstract block (see UV_ALLOC_CHECK notes below)
#define UV_UNWRAP(v, t) ((t *)Field(v, 0))

#define Connect_val(v) UV_UNWRAP(v, uv_connect_t)
#define Fs_val(v) UV_UNWRAP(v, uv_fs_t)
#define FsEvent_val(v) UV_UNWRAP(v, uv_fs_event_t)
#define GetAddrInfo_val(v) UV_UNWRAP(v, uv_getaddrinfo_t)
#define Handle_val(v) UV_UNWRAP(v, uv_handle_t)
#define Loop_val(v) UV_UNWRAP(v, uv_loop_t)
#define Pipe_val(v) UV_UNWRAP(v, uv_pipe_t)
#define Process_val(v) UV_UNWRAP(v, uv_process_t)
#define Shutdown_val(v) UV_UNWRAP(v, uv_shutdown_t)
#define Stream_val(v) UV_UNWRAP(v, uv_stream_t)
#define Tcp_val(v) UV_UNWRAP(v, uv_tcp_t)
#define Timer_val(v) UV_UNWRAP(v, uv_timer_t)
#define Udp_val(v) UV_UNWRAP(v, uv_udp_t)
#define UdpSend_val(v) UV_UNWRAP(v, uv_udp_send_t)
#define Write_val(v) UV_UNWRAP(v, uv_write_t)

// (no-op) typecast to juggle value and uv_file (which is an unboxed integer)
#define Val_file(f) ((value)(f))
#define File_val(v) ((uv_file)(v))

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
#define BC_WRAP8(name) \
	CAMLprim value name ## _bytecode(value *argv, int argc) { \
		return name(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]); \
	}
#define BC_WRAP9(name) \
	CAMLprim value name ## _bytecode(value *argv, int argc) { \
		return name(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]); \
	}
#define BC_WRAP10(name) \
	CAMLprim value name ## _bytecode(value *argv, int argc) { \
		return name(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9]); \
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
	UV_ERROR_CHECK_C(uv_loop_init(Loop_val(loop)), free(Loop_val(loop)));
	UV_SUCCESS(loop);
}

CAMLprim value w_loop_close(value loop) {
	CAMLparam1(loop);
	UV_ERROR_CHECK(uv_loop_close(Loop_val(loop)));
	free(Loop_val(loop));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_run(value loop, value mode) {
	CAMLparam2(loop, mode);
	UV_SUCCESS(Val_bool(uv_run(Loop_val(loop), (uv_run_mode)Int_val(mode))));
}

CAMLprim value w_stop(value loop) {
	CAMLparam1(loop);
	uv_stop(Loop_val(loop));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_loop_alive(value loop) {
	CAMLparam1(loop);
	UV_SUCCESS(Val_bool(uv_loop_alive(Loop_val(loop)) != 0));
}

// ------------- FILESYSTEM -----------------------------------------

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
		UV_FREE_REQ(req); \
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
		value2 = caml_alloc(2, 0);
		CAMLlocal3(cur, dirent, node);
		cur = value2;
		while (uv_fs_scandir_next(req, &ent) != UV_EOF) {
			dirent = caml_alloc(2, 0);
			Store_field(dirent, 0, caml_copy_string(ent.name));
			Store_field(dirent, 1, Val_int(ent.type));
			node = caml_alloc(2, 0);
			Store_field(node, 0, dirent);
			Store_field(cur, 1, node);
			cur = node;
		}
		Store_field(cur, 1, Val_unit);
		value2 = Field(value2, 1);
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
		UV_ALLOC_REQ(req, uv_fs_t, cb); \
		precall \
		UV_ERROR_CHECK_C(uv_ ## name(Loop_val(loop), Fs_val(req), call, handler), UV_FREE_REQ(Fs_val(req))); \
		UV_SUCCESS_UNIT; \
	} \
	CAMLprim value w_ ## name ## _sync(value loop, sign) { \
		CAMLparam1(loop); \
		locals; \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		precall \
		UV_ERROR_CHECK_C(uv_ ## name(Loop_val(loop), Fs_val(req), call, NULL), free(Fs_val(req))); \
		UV_ERROR_CHECK_C(Fs_val(req)->result, { uv_fs_req_cleanup(Fs_val(req)); free(Fs_val(req)); }); \
		CAMLlocal1(ret); \
		ret = handler ## _sync(Fs_val(req)); \
		uv_fs_req_cleanup(Fs_val(req)); \
		free(Fs_val(req)); \
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

// ------------- HANDLE DATA ----------------------------------------

/**
	There is a single `void *data` field on requests and handles. For requests,
	we use this to directly store the `value` for the callback function. For
	handles, however, it is sometimes necessary to register multiple different
	callbacks, hence a separate allocated struct is needed to hold them all.
	All of the fields of the struct are registered with the garbage collector
	immediately upon creation, although initially some of the callback fields are
	set to unit values.
**/

#define UV_HANDLE_DATA(h) (((uv_handle_t *)(h))->data)
#define UV_HANDLE_DATA_SUB(h, t) (((uv_w_handle_t *)UV_HANDLE_DATA(h))->u.t)

typedef struct {
	value cb_close;
	union {
		struct {
			value cb1;
			value cb2;
		} all;
		struct {
			value cb_fs_event;
			value unused1;
		} fs_event;
		struct {
			value cb_read;
			value cb_connection;
		} stream;
		struct {
			value cb_read;
			value cb_connection;
		} tcp;
		struct {
			value cb_read;
			value unused1;
		} udp;
		struct {
			value cb_timer;
			value unused1;
		} timer;
		struct {
			value cb_exit;
			value unused1;
		} process;
		struct {
			value unused1;
			value unused2;
		} pipe;
	} u;
} uv_w_handle_t;

static uv_w_handle_t *alloc_data_fs_event(value cb_fs_event) {
	uv_w_handle_t *data = calloc(1, sizeof(uv_w_handle_t));
	if (data != NULL) {
		data->cb_close = Val_unit;
		caml_register_global_root(&(data->cb_close));
		data->u.fs_event.cb_fs_event = cb_fs_event;
		caml_register_global_root(&(data->u.fs_event.cb_fs_event));
	}
	return data;
}

static uv_w_handle_t *alloc_data_tcp(value cb_read, value cb_connection) {
	uv_w_handle_t *data = calloc(1, sizeof(uv_w_handle_t));
	if (data != NULL) {
		data->cb_close = Val_unit;
		caml_register_global_root(&(data->cb_close));
		data->u.tcp.cb_read = cb_read;
		caml_register_global_root(&(data->u.tcp.cb_read));
		data->u.tcp.cb_connection = cb_connection;
		caml_register_global_root(&(data->u.tcp.cb_connection));
	}
	return data;
}

static uv_w_handle_t *alloc_data_udp(value cb_read) {
	uv_w_handle_t *data = calloc(1, sizeof(uv_w_handle_t));
	if (data != NULL) {
		data->cb_close = Val_unit;
		caml_register_global_root(&(data->cb_close));
		data->u.udp.cb_read = cb_read;
		caml_register_global_root(&(data->u.udp.cb_read));
	}
	return data;
}

static uv_w_handle_t *alloc_data_timer(value cb_timer) {
	uv_w_handle_t *data = calloc(1, sizeof(uv_w_handle_t));
	if (data != NULL) {
		data->cb_close = Val_unit;
		caml_register_global_root(&(data->cb_close));
		data->u.timer.cb_timer = cb_timer;
		caml_register_global_root(&(data->u.timer.cb_timer));
	}
	return data;
}

static uv_w_handle_t *alloc_data_process(value cb_exit) {
	uv_w_handle_t *data = calloc(1, sizeof(uv_w_handle_t));
	if (data != NULL) {
		data->cb_close = Val_unit;
		caml_register_global_root(&(data->cb_close));
		data->u.process.cb_exit = cb_exit;
		caml_register_global_root(&(data->u.process.cb_exit));
	}
	return data;
}

static uv_w_handle_t *alloc_data_pipe(void) {
	uv_w_handle_t *data = calloc(1, sizeof(uv_w_handle_t));
	if (data != NULL) {
		data->cb_close = Val_unit;
		caml_register_global_root(&(data->cb_close));
	}
	return data;
}

static void unalloc_data(uv_w_handle_t *data) {
	caml_remove_global_root(&(data->cb_close));
	caml_remove_global_root(&(data->u.all.cb1));
	caml_remove_global_root(&(data->u.all.cb2));
	free(data);
}

static void handle_close_cb(uv_handle_t *handle) {
	CAMLparam0();
	CAMLlocal2(cb, res);
	cb = ((uv_w_handle_t *)UV_HANDLE_DATA(handle))->cb_close;
	unalloc_data(UV_HANDLE_DATA(handle));
	free(handle);
	res = caml_alloc(1, 1);
	Store_field(res, 0, Val_unit);
	caml_callback(cb, res);
	CAMLreturn0;
}

CAMLprim value w_close(value handle, value cb) {
	CAMLparam2(handle, cb);
	((uv_w_handle_t *)UV_HANDLE_DATA(Handle_val(handle)))->cb_close = cb;
	uv_close(Handle_val(handle), handle_close_cb);
	UV_SUCCESS_UNIT;
}

CAMLprim value w_ref(value handle) {
	CAMLparam1(handle);
	uv_ref(Handle_val(handle));
	CAMLreturn(Val_unit);
}

CAMLprim value w_unref(value handle) {
	CAMLparam1(handle);
	uv_unref(Handle_val(handle));
	CAMLreturn(Val_unit);
}

// ------------- FILESYSTEM EVENTS ----------------------------------

static void handle_fs_event_cb(uv_fs_event_t *handle, const char *filename, int events, int status) {
	CAMLparam0();
	CAMLlocal2(cb, res);
	cb = UV_HANDLE_DATA_SUB(handle, fs_event).cb_fs_event;
	res = caml_alloc(1, status < 0 ? 0 : 1);
	if (status < 0)
		Store_field(res, 0, Val_int(status));
	else {
		CAMLlocal1(event);
		event = caml_alloc(2, 0);
		Store_field(event, 0, caml_copy_string(filename));
		Store_field(event, 1, Val_int(events));
		Store_field(res, 0, event);
	}
	caml_callback(cb, res);
	CAMLreturn0;
}

CAMLprim value w_fs_event_start(value loop, value path, value recursive, value cb) {
	CAMLparam4(loop, path, recursive, cb);
	UV_ALLOC_CHECK(handle, uv_fs_event_t);
	UV_ERROR_CHECK_C(uv_fs_event_init(Loop_val(loop), FsEvent_val(handle)), free(FsEvent_val(handle)));
	UV_HANDLE_DATA(FsEvent_val(handle)) = alloc_data_fs_event(cb);
	if (UV_HANDLE_DATA(FsEvent_val(handle)) == NULL)
		UV_ERROR(0);
	UV_ERROR_CHECK_C(
		uv_fs_event_start(FsEvent_val(handle), handle_fs_event_cb, String_val(path), Bool_val(recursive) ? UV_FS_EVENT_RECURSIVE : 0),
		{ unalloc_data(UV_HANDLE_DATA(FsEvent_val(handle))); free(FsEvent_val(handle)); }
		);
	UV_SUCCESS(handle);
}

CAMLprim value w_fs_event_stop(value handle, value cb) {
	CAMLparam2(handle, cb);
	UV_ERROR_CHECK_C(
		uv_fs_event_stop(FsEvent_val(handle)),
		{ unalloc_data(UV_HANDLE_DATA(FsEvent_val(handle))); free(FsEvent_val(handle)); }
		);
	((uv_w_handle_t *)UV_HANDLE_DATA(FsEvent_val(handle)))->cb_close = cb;
	uv_close(Handle_val(handle), handle_close_cb);
	UV_SUCCESS_UNIT;
}

// ------------- STREAM ---------------------------------------------

static void handle_stream_cb(uv_req_t *req, int status) {
	CAMLparam0();
	CAMLlocal2(cb, res);
	cb = (value)UV_REQ_DATA(req);
	res = caml_alloc(1, status < 0 ? 0 : 1);
	if (status < 0)
		Store_field(res, 0, Val_int(status));
	else
		Store_field(res, 0, Val_unit);
	caml_callback(cb, res);
	UV_FREE_REQ(req);
	CAMLreturn0;
}

static void handle_stream_cb_connection(uv_stream_t *stream, int status) {
	CAMLparam0();
	CAMLlocal2(cb, res);
	cb = UV_HANDLE_DATA_SUB(stream, stream).cb_connection;
	res = caml_alloc(1, status < 0 ? 0 : 1);
	if (status < 0)
		Store_field(res, 0, Val_int(status));
	else
		Store_field(res, 0, Val_unit);
	caml_callback(cb, res);
	CAMLreturn0;
}

static void handle_stream_cb_alloc(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
	buf->base = malloc(suggested_size);
	buf->len = suggested_size;
}

static void handle_stream_cb_read(uv_stream_t *stream, long int nread, const uv_buf_t *buf) {
	CAMLparam0();
	CAMLlocal2(cb, res);
	cb = UV_HANDLE_DATA_SUB(stream, stream).cb_read;
	res = caml_alloc(1, nread < 0 ? 0 : 1);
	if (nread < 0)
		Store_field(res, 0, Val_int(nread));
	else {
		CAMLlocal1(bytes);
		/**
			FIXME: libuv will not reuse the buffer `buf` after this (we `free` it).
			Ideally we could allocate an OCaml `bytes` value and make it reference
			the buffer base directly.
			Alternatively, in `handle_stream_cb_alloc` we allocate an OCaml string,
			then trim it somehow.
			For now, we do a `memcpy` of each buffer.
		**/
		bytes = caml_alloc_string(nread);
		if (buf->base != NULL) {
			if (nread > 0)
				memcpy(&Byte(bytes, 0), buf->base, nread);
			free(buf->base);
		}
		Store_field(res, 0, bytes);
	}
	caml_callback(cb, res);
	CAMLreturn0;
}

CAMLprim value w_shutdown(value stream, value cb) {
	CAMLparam2(stream, cb);
	UV_ALLOC_REQ(req, uv_shutdown_t, cb);
	UV_ERROR_CHECK_C(uv_shutdown(Shutdown_val(req), Stream_val(stream), (void (*)(uv_shutdown_t *, int))handle_stream_cb), UV_FREE_REQ(Shutdown_val(req)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_listen(value stream, value backlog, value cb) {
	CAMLparam3(stream, backlog, cb);
	UV_HANDLE_DATA_SUB(Stream_val(stream), stream).cb_connection = cb;
	UV_ERROR_CHECK(uv_listen(Stream_val(stream), Int_val(backlog), handle_stream_cb_connection));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_write(value stream, value data, value cb) {
	CAMLparam3(stream, data, cb);
	UV_ALLOC_REQ(req, uv_write_t, cb);
	uv_buf_t buf = uv_buf_init(&Byte(data, 0), caml_string_length(data));
	UV_ERROR_CHECK_C(uv_write(Write_val(req), Stream_val(stream), &buf, 1, (void (*)(uv_write_t *, int))handle_stream_cb), UV_FREE_REQ(Write_val(req)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_read_start(value stream, value cb) {
	CAMLparam2(stream, cb);
	UV_HANDLE_DATA_SUB(Stream_val(stream), stream).cb_read = cb;
	UV_ERROR_CHECK(uv_read_start(Stream_val(stream), handle_stream_cb_alloc, handle_stream_cb_read));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_read_stop(value stream) {
	CAMLparam1(stream);
	UV_ERROR_CHECK(uv_read_stop(Stream_val(stream)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_stream_of_handle(value handle) {
	CAMLparam1(handle);
	CAMLreturn(handle);
}

// ------------- NETWORK MACROS -------------------------------------

#define UV_SOCKADDR_IPV4(var, host, port) \
	struct sockaddr_in var; \
	var.sin_family = AF_INET; \
	var.sin_port = htons((unsigned short)port); \
	var.sin_addr.s_addr = htonl(host);
#define UV_SOCKADDR_IPV6(var, host, port) \
	struct sockaddr_in6 var; \
	memset(&var, 0, sizeof(var)); \
	var.sin6_family = AF_INET6; \
	var.sin6_port = htons((unsigned short)port); \
	memcpy(var.sin6_addr.s6_addr, host, 16);

// ------------- TCP ------------------------------------------------

CAMLprim value w_tcp_init(value loop) {
	CAMLparam1(loop);
	UV_ALLOC_CHECK(handle, uv_tcp_t);
	UV_ERROR_CHECK_C(uv_tcp_init(Loop_val(loop), Tcp_val(handle)), free(Tcp_val(handle)));
	UV_HANDLE_DATA(Tcp_val(handle)) = alloc_data_tcp(Val_unit, Val_unit);
	if (UV_HANDLE_DATA(Tcp_val(handle)) == NULL)
		UV_ERROR(0);
	UV_SUCCESS(handle);
}

CAMLprim value w_tcp_nodelay(value handle, value enable) {
	CAMLparam2(handle, enable);
	UV_ERROR_CHECK(uv_tcp_nodelay(Tcp_val(handle), Bool_val(enable)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_tcp_keepalive(value handle, value enable, value delay) {
	CAMLparam3(handle, enable, delay);
	UV_ERROR_CHECK(uv_tcp_keepalive(Tcp_val(handle), Bool_val(enable), Int_val(delay)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_tcp_accept(value loop, value server) {
	CAMLparam2(loop, server);
	UV_ALLOC_CHECK(client, uv_tcp_t);
	UV_ERROR_CHECK_C(uv_tcp_init(Loop_val(loop), Tcp_val(client)), free(Tcp_val(client)));
	UV_HANDLE_DATA(Tcp_val(client)) = alloc_data_tcp(Val_unit, Val_unit);
	if (UV_HANDLE_DATA(Tcp_val(client)) == NULL)
		UV_ERROR(0);
	UV_ERROR_CHECK_C(uv_accept(Stream_val(server), Stream_val(client)), free(Tcp_val(client)));
	UV_SUCCESS(client);
}

CAMLprim value w_tcp_bind_ipv4(value handle, value host, value port) {
	CAMLparam3(handle, host, port);
	UV_SOCKADDR_IPV4(addr, Int_val(host), Int_val(port));
	UV_ERROR_CHECK(uv_tcp_bind(Tcp_val(handle), (const struct sockaddr *)&addr, 0));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_tcp_bind_ipv6(value handle, value host, value port, value ipv6only) {
	CAMLparam3(handle, host, port);
	UV_SOCKADDR_IPV6(addr, &Byte(host, 0), Int_val(port));
	UV_ERROR_CHECK(uv_tcp_bind(Tcp_val(handle), (const struct sockaddr *)&addr, Bool_val(ipv6only) ? UV_TCP_IPV6ONLY : 0));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_tcp_connect_ipv4(value handle, value host, value port, value cb) {
	CAMLparam4(handle, host, port, cb);
	UV_SOCKADDR_IPV4(addr, Int_val(host), Int_val(port));
	UV_ALLOC_REQ(req, uv_connect_t, cb);
	UV_ERROR_CHECK_C(uv_tcp_connect(Connect_val(req), Tcp_val(handle), (const struct sockaddr *)&addr, (void (*)(uv_connect_t *, int))handle_stream_cb), UV_FREE_REQ(Connect_val(req)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_tcp_connect_ipv6(value handle, value host, value port, value cb) {
	CAMLparam4(handle, host, port, cb);
	UV_SOCKADDR_IPV6(addr, &Byte(host, 0), Int_val(port));
	UV_ALLOC_REQ(req, uv_connect_t, cb);
	UV_ERROR_CHECK_C(uv_tcp_connect(Connect_val(req), Tcp_val(handle), (const struct sockaddr *)&addr, (void (*)(uv_connect_t *, int))handle_stream_cb), UV_FREE_REQ(Connect_val(req)));
	UV_SUCCESS_UNIT;
}

static value w_getname(struct sockaddr_storage *storage) {
	CAMLparam0();
	CAMLlocal3(res, addr, infostore);
	res = caml_alloc(2, 0);
	if (storage->ss_family == AF_INET) {
		addr = caml_alloc(1, 0);
		Store_field(addr, 0, caml_copy_int32(ntohl(((struct sockaddr_in *)storage)->sin_addr.s_addr)));
		Store_field(res, 1, Val_int(ntohs(((struct sockaddr_in *)storage)->sin_port)));
	} else if (storage->ss_family == AF_INET6) {
		addr = caml_alloc(1, 1);
		infostore = caml_alloc_string(sizeof(struct in6_addr));
		memcpy(&Byte(infostore, 0), ((struct sockaddr_in6 *)storage)->sin6_addr.s6_addr, sizeof(struct in6_addr));
		Store_field(addr, 0, infostore);
		Store_field(res, 1, Val_int(ntohs(((struct sockaddr_in6 *)storage)->sin6_port)));
	} else {
		UV_ERROR(0);
	}
	Store_field(res, 0, addr);
	UV_SUCCESS(res);
}

CAMLprim value w_tcp_getsockname(value handle) {
	CAMLparam1(handle);
	struct sockaddr_storage storage;
	int dummy = sizeof(struct sockaddr_storage);
	UV_ERROR_CHECK(uv_tcp_getsockname(Tcp_val(handle), (struct sockaddr *)&storage, &dummy));
	CAMLreturn(w_getname(&storage));
}

CAMLprim value w_tcp_getpeername(value handle) {
	CAMLparam1(handle);
	struct sockaddr_storage storage;
	int dummy = sizeof(struct sockaddr_storage);
	UV_ERROR_CHECK(uv_tcp_getpeername(Tcp_val(handle), (struct sockaddr *)&storage, &dummy));
	CAMLreturn(w_getname(&storage));
}

// ------------- UDP ------------------------------------------------

static void handle_udp_cb_recv(uv_udp_t *handle, long int nread, const uv_buf_t *buf, const struct sockaddr *addr, unsigned int flags) {
	CAMLparam0();
	CAMLlocal2(cb, res);
	cb = UV_HANDLE_DATA_SUB(handle, udp).cb_read;
	res = caml_alloc(1, nread < 0 ? 0 : 1);
	if (nread < 0)
		Store_field(res, 0, Val_int(nread));
	else {
		CAMLlocal4(message, message_addr, message_addr_store, bytes);
		message = caml_alloc(3, 0);
		// FIXME: see comment in `handle_stream_cb_read`.
		bytes = caml_alloc_string(nread);
		if (buf->base != NULL) {
			if (nread > 0)
				memcpy(&Byte(bytes, 0), buf->base, nread);
			free(buf->base);
		}
		Store_field(message, 0, bytes);
		if (addr->sa_family == AF_INET) {
			message_addr = caml_alloc(1, 0);
			Store_field(message_addr, 0, caml_copy_int32(ntohl(((struct sockaddr_in *)addr)->sin_addr.s_addr)));
			Store_field(message, 2, Val_int(ntohs(((struct sockaddr_in *)addr)->sin_port)));
		} else if (addr->sa_family == AF_INET6) {
			message_addr = caml_alloc(1, 1);
			message_addr_store = caml_alloc_string(sizeof(struct in6_addr));
			memcpy(&Byte(message_addr_store, 0), ((struct sockaddr_in6 *)addr)->sin6_addr.s6_addr, sizeof(struct in6_addr));
			Store_field(message_addr, 0, message_addr_store);
			Store_field(message, 2, Val_int(ntohs(((struct sockaddr_in6 *)addr)->sin6_port)));
		}
		Store_field(message, 1, message_addr);
		Store_field(res, 0, message);
	}
	caml_callback(cb, res);
	CAMLreturn0;
}

CAMLprim value w_udp_init(value loop) {
	CAMLparam1(loop);
	UV_ALLOC_CHECK(handle, uv_udp_t);
	UV_ERROR_CHECK_C(uv_udp_init(Loop_val(loop), Udp_val(handle)), free(Udp_val(handle)));
	UV_HANDLE_DATA(Udp_val(handle)) = alloc_data_udp(Val_unit);
	if (UV_HANDLE_DATA(Udp_val(handle)) == NULL)
		UV_ERROR(0);
	UV_SUCCESS(handle);
}

CAMLprim value w_udp_bind_ipv4(value handle, value host, value port) {
	CAMLparam3(handle, host, port);
	UV_SOCKADDR_IPV4(addr, Int_val(host), Int_val(port));
	UV_ERROR_CHECK(uv_udp_bind(Udp_val(handle), (const struct sockaddr *)&addr, 0));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_bind_ipv6(value handle, value host, value port, value ipv6only) {
	CAMLparam3(handle, host, port);
	UV_SOCKADDR_IPV6(addr, &Byte(host, 0), Int_val(port));
	UV_ERROR_CHECK(uv_udp_bind(Udp_val(handle), (const struct sockaddr *)&addr, Bool_val(ipv6only) ? UV_UDP_IPV6ONLY : 0));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_send_ipv4(value handle, value msg, value offset, value length, value host, value port, value cb) {
	CAMLparam5(handle, msg, offset, length, host);
	CAMLxparam2(port, cb);
	UV_SOCKADDR_IPV4(addr, Int_val(host), Int_val(port));
	UV_ALLOC_REQ(req, uv_udp_send_t, cb);
	uv_buf_t buf = uv_buf_init(&Byte(msg, Int_val(offset)), Int_val(length));
	UV_ERROR_CHECK_C(uv_udp_send(UdpSend_val(req), Udp_val(handle), &buf, 1, (const struct sockaddr *)&addr, (void (*)(uv_udp_send_t *, int))handle_stream_cb), UV_FREE_REQ(UdpSend_val(req)));
	UV_SUCCESS_UNIT;
}
BC_WRAP7(w_udp_send_ipv4);

CAMLprim value w_udp_send_ipv6(value handle, value msg, value offset, value length, value host, value port, value cb) {
	CAMLparam5(handle, msg, offset, length, host);
	CAMLxparam2(port, cb);
	UV_SOCKADDR_IPV6(addr, &Byte(host, 0), Int_val(port));
	UV_ALLOC_REQ(req, uv_udp_send_t, cb);
	uv_buf_t buf = uv_buf_init(&Byte(msg, Int_val(offset)), Int_val(length));
	UV_ERROR_CHECK_C(uv_udp_send(UdpSend_val(req), Udp_val(handle), &buf, 1, (const struct sockaddr *)&addr, (void (*)(uv_udp_send_t *, int))handle_stream_cb), UV_FREE_REQ(UdpSend_val(req)));
	UV_SUCCESS_UNIT;
}
BC_WRAP7(w_udp_send_ipv6);

CAMLprim value w_udp_recv_start(value handle, value cb) {
	CAMLparam2(handle, cb);
	UV_HANDLE_DATA_SUB(Udp_val(handle), udp).cb_read = cb;
	UV_ERROR_CHECK(uv_udp_recv_start(Udp_val(handle), handle_stream_cb_alloc, handle_udp_cb_recv));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_recv_stop(value handle) {
	CAMLparam1(handle);
	UV_ERROR_CHECK(uv_udp_recv_stop(Udp_val(handle)));
	UV_HANDLE_DATA_SUB(Udp_val(handle), udp).cb_read = Val_unit;
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_set_membership(value handle, value address, value intfc, value join) {
	CAMLparam4(handle, address, intfc, join);
	const char *intfc_u = NULL;
	if (caml_string_length(intfc) != 0)
		intfc_u = String_val(intfc);
	UV_ERROR_CHECK(uv_udp_set_membership(Udp_val(handle), String_val(address), intfc_u, Bool_val(join) ? UV_JOIN_GROUP : UV_LEAVE_GROUP));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_close(value handle, value cb) {
	return w_close(handle, cb);
}

CAMLprim value w_udp_getsockname(value handle) {
	CAMLparam1(handle);
	struct sockaddr_storage storage;
	int dummy = sizeof(struct sockaddr_storage);
	UV_ERROR_CHECK(uv_udp_getsockname(Udp_val(handle), (struct sockaddr *)&storage, &dummy));
	CAMLreturn(w_getname(&storage));
}

CAMLprim value w_udp_set_broadcast(value handle, value flag) {
	CAMLparam2(handle, flag);
	UV_ERROR_CHECK(uv_udp_set_broadcast(Udp_val(handle), Bool_val(flag)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_set_multicast_interface(value handle, value intfc) {
	CAMLparam2(handle, intfc);
	UV_ERROR_CHECK(uv_udp_set_multicast_interface(Udp_val(handle), String_val(intfc)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_set_multicast_loopback(value handle, value flag) {
	CAMLparam2(handle, flag);
	UV_ERROR_CHECK(uv_udp_set_multicast_loop(Udp_val(handle), Bool_val(flag) ? 1 : 0));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_set_multicast_ttl(value handle, value ttl) {
	CAMLparam2(handle, ttl);
	UV_ERROR_CHECK(uv_udp_set_multicast_ttl(Udp_val(handle), Int_val(ttl)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_set_ttl(value handle, value ttl) {
	CAMLparam2(handle, ttl);
	UV_ERROR_CHECK(uv_udp_set_ttl(Udp_val(handle), Int_val(ttl)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_udp_get_recv_buffer_size(value handle) {
	CAMLparam1(handle);
	int size_u = 0;
	int res = uv_recv_buffer_size(Handle_val(handle), &size_u);
	CAMLreturn(Val_int(res));
}

CAMLprim value w_udp_get_send_buffer_size(value handle) {
	CAMLparam1(handle);
	int size_u = 0;
	int res = uv_send_buffer_size(Handle_val(handle), &size_u);
	CAMLreturn(Val_int(res));
}

CAMLprim value w_udp_set_recv_buffer_size(value handle, value size) {
	CAMLparam2(handle, size);
	int size_u = Int_val(size);
	int res = uv_recv_buffer_size(Handle_val(handle), &size_u);
	CAMLreturn(Val_int(res));
}

CAMLprim value w_udp_set_send_buffer_size(value handle, value size) {
	CAMLparam2(handle, size);
	int size_u = Int_val(size);
	int res = uv_send_buffer_size(Handle_val(handle), &size_u);
	CAMLreturn(Val_int(res));
}

// ------------- DNS ------------------------------------------------

static void handle_dns_gai_cb(uv_getaddrinfo_t *req, int status, struct addrinfo *gai_res) {
	CAMLparam0();
	CAMLlocal2(cb, res);
	cb = (value)UV_REQ_DATA(req);
	res = caml_alloc(1, status < 0 ? 0 : 1);
	if (status < 0)
		Store_field(res, 0, Val_int(status));
	else {
		CAMLlocal5(infos, cur, info, node, infostore);
		infos = caml_alloc(2, 0);
		cur = infos;
		struct addrinfo *gai_cur = gai_res;
		while (gai_cur != NULL) {
			if (gai_cur->ai_family == AF_INET) {
				info = caml_alloc(1, 0);
				Store_field(info, 0, caml_copy_int32(ntohl(((struct sockaddr_in *)gai_cur->ai_addr)->sin_addr.s_addr)));
			} else if (gai_cur->ai_family == AF_INET6) {
				info = caml_alloc(1, 1);
				infostore = caml_alloc_string(sizeof(struct in6_addr));
				memcpy(&Byte(infostore, 0), ((struct sockaddr_in6 *)gai_cur->ai_addr)->sin6_addr.s6_addr, sizeof(struct in6_addr));
				Store_field(info, 0, infostore);
			} else {
				gai_cur = gai_cur->ai_next;
				continue;
			}
			gai_cur = gai_cur->ai_next;
			node = caml_alloc(2, 0);
			Store_field(node, 0, info);
			Store_field(cur, 1, node);
			cur = node;
		}
		Store_field(cur, 1, Val_unit);
		infos = Field(infos, 1);
		uv_freeaddrinfo(gai_res);
		Store_field(res, 0, infos);
	}
	caml_callback(cb, res);
	UV_FREE_REQ(req);
	CAMLreturn0;
}

CAMLprim value w_dns_getaddrinfo(value loop, value node, value flag_addrconfig, value flag_v4mapped, value hint_family, value cb) {
	CAMLparam5(loop, node, flag_addrconfig, flag_v4mapped, hint_family);
	CAMLxparam1(cb);
	UV_ALLOC_REQ(req, uv_getaddrinfo_t, cb);
	int hint_flags_u = 0;
	if (Bool_val(flag_addrconfig))
		hint_flags_u |= AI_ADDRCONFIG;
	if (Bool_val(flag_v4mapped))
		hint_flags_u |= AI_V4MAPPED;
	int hint_family_u = AF_UNSPEC;
	if (Int_val(hint_family) == 4)
		hint_family_u = AF_INET;
	else if (Int_val(hint_family) == 6)
		hint_family_u = AF_INET6;
	struct addrinfo hints = {
		.ai_flags = hint_flags_u,
		.ai_family = hint_family_u,
		.ai_socktype = 0,
		.ai_protocol = 0,
		.ai_addrlen = 0,
		.ai_addr = NULL,
		.ai_canonname = NULL,
		.ai_next = NULL
	};
	UV_ERROR_CHECK_C(uv_getaddrinfo(Loop_val(loop), GetAddrInfo_val(req), handle_dns_gai_cb, &Byte(node, 0), NULL, &hints), UV_FREE_REQ(GetAddrInfo_val(req)));
	UV_SUCCESS_UNIT;
}
BC_WRAP6(w_dns_getaddrinfo);

// ------------- TIMERS ---------------------------------------------

static void handle_timer_cb(uv_timer_t *handle) {
	CAMLparam0();
	CAMLlocal1(cb);
	cb = UV_HANDLE_DATA_SUB(handle, timer).cb_timer;
	caml_callback(cb, Val_unit);
	CAMLreturn0;
}

CAMLprim value w_timer_start(value loop, value timeout, value cb) {
	CAMLparam3(loop, timeout, cb);
	UV_ALLOC_CHECK(handle, uv_timer_t);
	UV_ERROR_CHECK_C(uv_timer_init(Loop_val(loop), Timer_val(handle)), free(Timer_val(handle)));
	UV_HANDLE_DATA(Timer_val(handle)) = alloc_data_timer(cb);
	if (UV_HANDLE_DATA(Timer_val(handle)) == NULL)
		UV_ERROR(0);
	UV_ERROR_CHECK_C(
		uv_timer_start(Timer_val(handle), handle_timer_cb, Int_val(timeout), Int_val(timeout)),
		{ unalloc_data(UV_HANDLE_DATA(Timer_val(handle))); free(Timer_val(handle)); }
		);
	UV_SUCCESS(handle);
}

CAMLprim value w_timer_stop(value handle, value cb) {
	CAMLparam2(handle, cb);
	UV_ERROR_CHECK_C(
		uv_timer_stop(Timer_val(handle)),
		{ unalloc_data(UV_HANDLE_DATA(Timer_val(handle))); free(Timer_val(handle)); }
		);
	((uv_w_handle_t *)UV_HANDLE_DATA(Timer_val(handle)))->cb_close = cb;
	uv_close(Handle_val(handle), handle_close_cb);
	UV_SUCCESS_UNIT;
}

// ------------- PROCESS --------------------------------------------

static void handle_process_cb(uv_process_t *handle, int64_t exit_status, int term_signal) {
	CAMLparam0();
	CAMLlocal3(cb, res, status);
	cb = UV_HANDLE_DATA_SUB(handle, process).cb_exit;
	res = caml_alloc(1, 1);
	status = caml_alloc(2, 0);
	Store_field(status, 0, Val_int(exit_status)); // FIXME: int64 -> int conversion
	Store_field(status, 1, Val_int(term_signal));
	Store_field(res, 0, status);
	caml_callback(cb, res);
	CAMLreturn0;
}

CAMLprim value w_spawn(value loop, value cb, value file, value args, value env, value cwd, value flags, value stdio, value uid, value gid) {
	CAMLparam5(loop, cb, file, args, env);
	CAMLxparam5(cwd, flags, stdio, uid, gid);
	UV_ALLOC_CHECK(handle, uv_process_t);
	UV_HANDLE_DATA(Process_val(handle)) = alloc_data_process(cb);
	if (UV_HANDLE_DATA(Process_val(handle)) == NULL)
		UV_ERROR(0);
	char **args_u = malloc(sizeof(char *) * (Wosize_val(args) + 1));
	for (int i = 0; i < Wosize_val(args); i++)
		args_u[i] = strdup(String_val(Field(args, i)));
	args_u[Wosize_val(args)] = NULL;
	char **env_u = malloc(sizeof(char *) * (Wosize_val(env) + 1));
	for (int i = 0; i < Wosize_val(env); i++)
		env_u[i] = strdup(String_val(Field(env, i)));
	env_u[Wosize_val(env)] = NULL;
	uv_stdio_container_t *stdio_u = malloc(sizeof(uv_stdio_container_t) * Wosize_val(stdio));
	CAMLlocal1(stdio_entry);
	for (int i = 0; i < Wosize_val(stdio); i++) {
		stdio_entry = Field(stdio, i);
		if (Is_long(stdio_entry)) {
			switch (Int_val(stdio_entry)) {
				case 0: // Ignore
					stdio_u[i].flags = UV_IGNORE;
					break;
				default: // 1, Inherit
					stdio_u[i].flags = UV_INHERIT_FD;
					stdio_u[i].data.fd = i;
					break;
			}
		} else {
			switch (Tag_val(stdio_entry)) {
				case 0: // Pipe
					stdio_u[i].flags = UV_CREATE_PIPE;
					if (Bool_val(Field(stdio_entry, 0)))
						stdio_u[i].flags |= UV_READABLE_PIPE;
					if (Bool_val(Field(stdio_entry, 1)))
						stdio_u[i].flags |= UV_WRITABLE_PIPE;
					stdio_u[i].data.stream = Stream_val(Field(stdio_entry, 2));
					break;
				default: // 1, Ipc
					stdio_u[i].flags = UV_CREATE_PIPE | UV_READABLE_PIPE | UV_WRITABLE_PIPE;
					stdio_u[i].data.stream = Stream_val(Field(stdio_entry, 0));
					break;
			}
		}
	}
	uv_process_options_t options = {
		.exit_cb = handle_process_cb,
		.file = String_val(file),
		.args = args_u,
		.env = env_u,
		.cwd = String_val(cwd),
		.flags = Int_val(flags),
		.stdio_count = Wosize_val(stdio),
		.stdio = stdio_u,
		.uid = Int_val(uid),
		.gid = Int_val(gid)
	};
	UV_ERROR_CHECK_C(
		uv_spawn(Loop_val(loop), Process_val(handle), &options),
		{ free(args_u); free(env_u); free(stdio_u); unalloc_data(UV_HANDLE_DATA(Process_val(handle))); free(Process_val(handle)); }
		);
	free(args_u);
	free(env_u);
	free(stdio_u);
	UV_SUCCESS(handle);
}
BC_WRAP10(w_spawn);

CAMLprim value w_process_kill(value handle, value signum, value cb) {
	// TODO: callback?
	CAMLparam2(handle, signum);
	UV_ERROR_CHECK(uv_process_kill(Process_val(handle), Int_val(signum)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_process_get_pid(value handle) {
	CAMLparam1(handle);
	CAMLreturn(Val_int(Process_val(handle)->pid));
}

// ------------- PIPES ----------------------------------------------

CAMLprim value w_pipe_init(value loop, value ipc) {
	CAMLparam2(loop, ipc);
	UV_ALLOC_CHECK(handle, uv_pipe_t);
	UV_ERROR_CHECK_C(uv_pipe_init(Loop_val(loop), Pipe_val(handle), Bool_val(ipc)), free(Pipe_val(handle)));
	UV_HANDLE_DATA(Pipe_val(handle)) = alloc_data_pipe();
	if (UV_HANDLE_DATA(Pipe_val(handle)) == NULL)
		UV_ERROR(0);
	UV_SUCCESS(handle);
}

CAMLprim value w_pipe_open(value pipe, value fd) {
	CAMLparam2(pipe, fd);
	UV_ERROR_CHECK(uv_pipe_open(Pipe_val(pipe), Int_val(fd)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_pipe_accept(value loop, value server) {
	CAMLparam2(loop, server);
	UV_ALLOC_CHECK(client, uv_pipe_t);
	UV_ERROR_CHECK_C(uv_pipe_init(Loop_val(loop), Pipe_val(client), 0), free(Pipe_val(client)));
	UV_HANDLE_DATA(Pipe_val(client)) = alloc_data_pipe();
	if (UV_HANDLE_DATA(Pipe_val(client)) == NULL)
		UV_ERROR(0);
	UV_ERROR_CHECK_C(uv_accept(Stream_val(server), Stream_val(client)), free(Pipe_val(client)));
	UV_SUCCESS(client);
}

CAMLprim value w_pipe_bind_ipc(value handle, value path) {
	CAMLparam2(handle, path);
	UV_ERROR_CHECK(uv_pipe_bind(Pipe_val(handle), String_val(path)));
	UV_SUCCESS_UNIT;
}

CAMLprim value w_pipe_connect_ipc(value handle, value path, value cb) {
	CAMLparam3(handle, path, cb);
	UV_ALLOC_REQ(req, uv_connect_t, cb);
	uv_pipe_connect(Connect_val(req), Pipe_val(handle), String_val(path), (void (*)(uv_connect_t *, int))handle_stream_cb);
	UV_SUCCESS_UNIT;
}

CAMLprim value w_pipe_pending_count(value handle) {
	CAMLparam1(handle);
	CAMLreturn(Val_int(uv_pipe_pending_count(Pipe_val(handle))));
}

CAMLprim value w_pipe_accept_pending(value loop, value handle) {
	CAMLparam2(loop, handle);
	CAMLlocal1(ret);
	switch (uv_pipe_pending_type(Pipe_val(handle))) {
		case UV_NAMED_PIPE: {
			ret = caml_alloc(1, 0);
			UV_ALLOC_CHECK(client, uv_pipe_t);
			UV_ERROR_CHECK_C(uv_pipe_init(Loop_val(loop), Pipe_val(client), 0), free(Pipe_val(client)));
			UV_HANDLE_DATA(Pipe_val(client)) = alloc_data_pipe();
			if (UV_HANDLE_DATA(Pipe_val(client)) == NULL)
				UV_ERROR(0);
			UV_ERROR_CHECK_C(uv_accept(Stream_val(handle), Stream_val(client)), free(Pipe_val(client)));
			Store_field(ret, 0, client);
		}; break;
		case UV_TCP: {
			ret = caml_alloc(1, 1);
			UV_ALLOC_CHECK(client, uv_tcp_t);
			UV_ERROR_CHECK_C(uv_tcp_init(Loop_val(loop), Tcp_val(client)), free(Tcp_val(client)));
			UV_HANDLE_DATA(Tcp_val(client)) = alloc_data_tcp(Val_unit, Val_unit);
			if (UV_HANDLE_DATA(Tcp_val(client)) == NULL)
				UV_ERROR(0);
			UV_ERROR_CHECK_C(uv_accept(Stream_val(handle), Stream_val(client)), free(Tcp_val(client)));
			Store_field(ret, 0, client);
		}; break;
		default:
			UV_ERROR(0);
			break;
	}
	UV_SUCCESS(ret);
}

CAMLprim value w_pipe_getsockname(value handle) {
	CAMLparam1(handle);
	char path[256];
	size_t path_size = 255;
	UV_ERROR_CHECK(uv_pipe_getsockname(Pipe_val(handle), path, &path_size));
	path[path_size] = 0;
	UV_SUCCESS(caml_copy_string(path));
}

CAMLprim value w_pipe_getpeername(value handle) {
	CAMLparam1(handle);
	char path[256];
	size_t path_size = 255;
	UV_ERROR_CHECK(uv_pipe_getpeername(Pipe_val(handle), path, &path_size));
	path[path_size] = 0;
	UV_SUCCESS(caml_copy_string(path));
}

CAMLprim value w_pipe_write_handle(value handle, value data, value send_handle, value cb) {
	CAMLparam4(handle, data, send_handle, cb);
	UV_ALLOC_REQ(req, uv_write_t, cb);
	uv_buf_t buf = uv_buf_init(&Byte(data, 0), caml_string_length(data));
	UV_ERROR_CHECK_C(uv_write2(Write_val(req), Stream_val(handle), &buf, 1, Stream_val(send_handle), (void (*)(uv_write_t *, int))handle_stream_cb), UV_FREE_REQ(Write_val(req)));
	UV_SUCCESS_UNIT;
}
