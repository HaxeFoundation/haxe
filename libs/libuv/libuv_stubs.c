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

// access the data of a handle or request
#define UV_HANDLE_DATA(h) (((uv_handle_t *)(h))->data)
#define UV_HANDLE_DATA_SUB(h, t) ((t *)((uv_handle_t *)(h))->data)
#define UV_REQ_DATA(r) (((uv_req_t *)(r))->data)
#define UV_REQ_DATA_A(r) ((value *)(&UV_REQ_DATA(r)))

#define UV_ALLOC(t) ((t *)malloc(sizeof(t)))

// ------------- ERROR HANDLING -------------------------------------

#define UV_ALLOC_CHECK(var, type) \
	type *var = UV_ALLOC(type); \
	if (var == NULL) { \
		caml_failwith("malloc " #type " failed"); \
	} else {}
#define UV_ALLOC_CHECK_C(var, type, cleanup) \
	type *var = UV_ALLOC(type); \
	if (var == NULL) { \
		cleanup; \
		caml_failwith("malloc " #type " failed"); \
	} else {}
// TODO: proper exceptions for libuv errors
#define UV_ERROR_CHECK(expr) do { \
		int __tmp_result = expr; \
		if (__tmp_result < 0) { \
			caml_failwith(strdup(uv_strerror(__tmp_result))); \
		} \
	} while (0)
#define UV_ERROR_CHECK_C(expr, cleanup) do { \
		int __tmp_result = expr; \
		if (__tmp_result < 0) { \
			cleanup; \
			caml_failwith(strdup(uv_strerror(__tmp_result))); \
		} \
	} while (0)

// ------------- LOOP -----------------------------------------------

CAMLprim value w_loop_init(value unit) {
	CAMLparam1(unit);
	UV_ALLOC_CHECK(loop, uv_loop_t);
	UV_ERROR_CHECK_C(uv_loop_init(loop), free(loop));
	CAMLreturn((value)loop);
}

CAMLprim value w_loop_close(value loop) {
	CAMLparam1(loop);
	UV_ERROR_CHECK(uv_loop_close((uv_loop_t *)loop));
	free((uv_loop_t *)loop);
	CAMLreturn(Val_unit);
}

CAMLprim value w_run(value loop, value mode) {
	CAMLparam2(loop, mode);
	CAMLreturn(Val_bool(uv_run((uv_loop_t *)loop, (uv_run_mode)mode) == 0));
}

CAMLprim value w_loop_alive(value loop) {
	CAMLparam1(loop);
	CAMLreturn(Val_bool(uv_loop_alive((uv_loop_t *)loop) != 0));
}

CAMLprim value w_stop(value loop) {
	CAMLparam1(loop);
	uv_stop((uv_loop_t *)loop);
	CAMLreturn(Val_unit);
}

// ------------- FILESYSTEM -----------------------------------------

// TODO: exception handling (optional arguments ...?)

static void handle_fs_cb(uv_fs_t *req) {
	CAMLparam0();
	value cb = (value)UV_REQ_DATA(req);
	if (req->result < 0)
		caml_failwith(uv_strerror(req->result));
		//hl_call1(void, cb, vdynamic *, construct_error((vbyte *)strdup(uv_strerror(req->result)), req->result));
	else
		caml_callback(cb, Val_unit);
	uv_fs_req_cleanup(req);
	caml_remove_global_root(UV_REQ_DATA_A(req));
	free(req);
	CAMLreturn0;
}

static value handle_fs_cb_sync(uv_fs_t *req) {
	return Val_unit;
}

#define UV_FS_HANDLER(name, setup) \
	static void name(uv_fs_t *req) { \
		CAMLparam0(); \
		value cb = (value)UV_REQ_DATA(req); \
		if (req->result < 0) \
			caml_failwith(uv_strerror(req->result)); \
		else { \
			value value2; \
			do setup while (0); \
			caml_callback(cb, value2); \
		} \
		uv_fs_req_cleanup(req); \
		caml_remove_global_root(UV_REQ_DATA_A(req)); \
		free(req); \
		CAMLreturn0; \
	} \
	static value name ## _sync(uv_fs_t *req) { \
		CAMLparam0(); \
		value value2; \
		do setup while (0); \
		CAMLreturn(value2); \
	}

UV_FS_HANDLER(handle_fs_cb_bytes, value2 = caml_copy_string((const char *)req->ptr););
UV_FS_HANDLER(handle_fs_cb_path, value2 = caml_copy_string((const char *)req->path););
UV_FS_HANDLER(handle_fs_cb_int, value2 = (value)req->result;);
UV_FS_HANDLER(handle_fs_cb_file, value2 = (value)req->result;);
/*UV_FS_HANDLER(handle_fs_cb_stat, value2 = construct_fs_stat(
		req->statbuf.st_dev,
		req->statbuf.st_mode,
		req->statbuf.st_nlink,
		req->statbuf.st_uid,
		req->statbuf.st_gid,
		req->statbuf.st_rdev,
		req->statbuf.st_ino,
		req->statbuf.st_size,
		req->statbuf.st_blksize,
		req->statbuf.st_blocks,
		req->statbuf.st_flags,
		req->statbuf.st_gen
	));*/
UV_FS_HANDLER(handle_fs_cb_scandir, {
		uv_dirent_t ent;
		value2 = Val_int(0);
		while (uv_fs_scandir_next(req, &ent) != UV_EOF) {
			value dirent = caml_alloc(2, 0);
			Store_field(dirent, 0, caml_copy_string(ent.name));
			Store_field(dirent, 1, ent.type);
			value node = caml_alloc(2, 0);
			Store_field(node, 0, dirent); // car
			Store_field(node, 1, value2); // cdr
			value2 = node;
		}
	});

	/*
#define UV_REQ_WRAP(name, reqtype, sign, call, handler) \
	CAMLprim value w_ ## name(sign, value cb) { \
		UV_ALLOC_CHECK(req, reqtype); \
		UV_REQ_DATA(req) = (void *)cb; \
		UV_ERROR_CHECK_C(uv_ ## name(req, call, handler), free(req)); \
		caml_register_global_root(UV_REQ_DATA(req)); \
		CAMLreturn0; \
	}
#define UV_REQ_WRAP_LOOP(name, reqtype, sign, call, ffi, handler) \
	CAMLprim value w_ ## name(value *loop, sign, value cb) { \
		UV_ALLOC_CHECK(req, reqtype); \
		UV_REQ_DATA(req) = (void *)cb; \
		UV_ERROR_CHECK_C(uv_ ## name(loop, req, call, handler), free(req)); \
		caml_register_global_root(UV_REQ_DATA(req)); \
		CAMLreturn0; \
	}
#define UV_REQ_WRAP_LOOP_SYNC(name, ret, reqtype, sign, call, ffiret, ffi, handler, doret) \
	CAMLprim value w_ ## name ## _sync(uv_loop_t *loop, sign) { \
		UV_ALLOC_CHECK(req, reqtype); \
		UV_ERROR_CHECK_C(uv_ ## name(loop, req, call, NULL), free(req)); \
		doret handler ## _sync(req); \
	}
	*/
/*
#define COMMA ,
#define FS_WRAP1_LOOP(name, ret, arg1, ffiret, ffi, ffihandler, handler, doret) \
	UV_REQ_WRAP_LOOP(name, uv_fs_t, arg1 _arg1, _arg1, ffi ffihandler, handler); \
	UV_REQ_WRAP_LOOP_SYNC(name, ret, uv_fs_t, arg1 _arg1, _arg1, ffiret, ffi, handler, doret)
#define FS_WRAP2_LOOP(name, ret, arg1, arg2, ffiret, ffi, ffihandler, handler, doret) \
	UV_REQ_WRAP_LOOP(name, uv_fs_t, arg1 _arg1 COMMA arg2 _arg2, _arg1 COMMA _arg2, ffi ffihandler, handler); \
	UV_REQ_WRAP_LOOP_SYNC(name, ret, uv_fs_t, arg1 _arg1 COMMA arg2 _arg2, _arg1 COMMA _arg2, ffiret, ffi, handler, doret)
#define FS_WRAP3_LOOP(name, ret, arg1, arg2, arg3, ffiret, ffi, ffihandler, handler, doret) \
	UV_REQ_WRAP_LOOP(name, uv_fs_t, arg1 _arg1 COMMA arg2 _arg2 COMMA arg3 _arg3, _arg1 COMMA _arg2 COMMA _arg3, ffi ffihandler, handler); \
	UV_REQ_WRAP_LOOP_SYNC(name, ret, uv_fs_t, arg1 _arg1 COMMA arg2 _arg2 COMMA arg3 _arg3, _arg1 COMMA _arg2 COMMA _arg3, ffiret, ffi, handler, doret)
#define FS_WRAP4_LOOP(name, ret, arg1, arg2, arg3, arg4, ffiret, ffi, ffihandler, handler, doret) \
	UV_REQ_WRAP_LOOP(name, uv_fs_t, arg1 _arg1 COMMA arg2 _arg2 COMMA arg3 _arg3 COMMA arg4 _arg4, _arg1 COMMA _arg2 COMMA _arg3 COMMA _arg4, ffi ffihandler, handler); \
	UV_REQ_WRAP_LOOP_SYNC(name, ret, uv_fs_t, arg1 _arg1 COMMA arg2 _arg2 COMMA arg3 _arg3 COMMA arg4 _arg4, _arg1 COMMA _arg2 COMMA _arg3 COMMA _arg4, ffiret, ffi, handler, doret)
*/

#define FS_WRAP1(name, arg1conv, handler) \
	CAMLprim value w_ ## name(value loop, value arg1, value cb) { \
		CAMLparam3(loop, arg1, cb); \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		UV_REQ_DATA(req) = (void *)cb; \
		caml_register_global_root(UV_REQ_DATA_A(req)); \
		UV_ERROR_CHECK_C(uv_ ## name((uv_loop_t *)loop, (uv_fs_t *)req, arg1conv(arg1), handler), free((uv_fs_t *)req)); \
		CAMLreturn(Val_unit); \
	} \
	CAMLprim value w_ ## name ## _sync(value loop, value arg1) { \
		CAMLparam2(loop, arg1); \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		caml_register_global_root(UV_REQ_DATA_A(req)); \
		UV_ERROR_CHECK_C(uv_ ## name((uv_loop_t *)loop, (uv_fs_t *)req, arg1conv(arg1), NULL), free((uv_fs_t *)req)); \
			UV_ERROR_CHECK_C(req->result, free(req));/* TODO: cleanup? */ \
		value ret = handler ## _sync(req); \
		uv_fs_req_cleanup(req); \
		free(req); \
		CAMLreturn(ret); \
	}

#define FS_WRAP2(name, arg1conv, arg2conv, handler) \
	CAMLprim value w_ ## name(value loop, value arg1, value arg2, value cb) { \
		CAMLparam4(loop, arg1, arg2, cb); \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		UV_REQ_DATA(req) = (void *)cb; \
		caml_register_global_root(UV_REQ_DATA_A(req)); \
		UV_ERROR_CHECK_C(uv_ ## name((uv_loop_t *)loop, (uv_fs_t *)req, arg1conv(arg1), arg2conv(arg2), handler), free((uv_fs_t *)req)); \
		CAMLreturn(Val_unit); \
	} \
	CAMLprim value w_ ## name ## _sync(value loop, value arg1, value arg2) { \
		CAMLparam3(loop, arg1, arg2); \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		caml_register_global_root(UV_REQ_DATA_A(req)); \
		UV_ERROR_CHECK_C(uv_ ## name((uv_loop_t *)loop, (uv_fs_t *)req, arg1conv(arg1), arg2conv(arg2), NULL), free((uv_fs_t *)req)); \
		UV_ERROR_CHECK_C(req->result, free(req));/* TODO: cleanup? */ \
		value ret = handler ## _sync(req); \
		uv_fs_req_cleanup(req); \
		free(req); \
		CAMLreturn(ret); \
	}

#define FS_WRAP3(name, arg1conv, arg2conv, arg3conv, handler) \
	CAMLprim value w_ ## name(value loop, value arg1, value arg2, value arg3, value cb) { \
		CAMLparam5(loop, arg1, arg2, arg3, cb); \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		UV_REQ_DATA(req) = (void *)cb; \
		caml_register_global_root(UV_REQ_DATA_A(req)); \
		UV_ERROR_CHECK_C(uv_ ## name((uv_loop_t *)loop, (uv_fs_t *)req, arg1conv(arg1), arg2conv(arg2), arg3conv(arg3), handler), free((uv_fs_t *)req)); \
		CAMLreturn(Val_unit); \
	} \
	CAMLprim value w_ ## name ## _sync(value loop, value arg1, value arg2, value arg3) { \
		CAMLparam4(loop, arg1, arg2, arg3); \
		UV_ALLOC_CHECK(req, uv_fs_t); \
		caml_register_global_root(UV_REQ_DATA_A(req)); \
		UV_ERROR_CHECK_C(uv_ ## name((uv_loop_t *)loop, (uv_fs_t *)req, arg1conv(arg1), arg2conv(arg2), arg3conv(arg3), NULL), free((uv_fs_t *)req)); \
		UV_ERROR_CHECK_C(req->result, free(req));/* TODO: cleanup? */ \
		value ret = handler ## _sync(req); \
		uv_fs_req_cleanup(req); \
		free(req); \
		CAMLreturn(ret); \
	}

/**
	FIXME:
		w_fs_read, w_fs_write, w_fs_read_sync, and w_fs_write_sync
		have a signature different from libuv due to no struct passing support in
		hashlink; currently only a single uv_buf_t can be passed at a time.
**/
	/*
HL_PRIM void HL_NAME(w_fs_read)(uv_loop_t *loop, uv_file file, const uv_buf_t *buf, int32_t offset, vclosure *cb) {
	UV_ALLOC_CHECK(req, uv_fs_t);
	UV_REQ_DATA(req) = (void *)cb;
	UV_ERROR_CHECK_C(uv_fs_read(loop, req, file, buf, 1, offset, handle_fs_cb_int), free(req));
	hl_add_root(UV_REQ_DATA(req));
}

HL_PRIM int HL_NAME(w_fs_read_sync)(uv_loop_t *loop, uv_file file, const uv_buf_t *buf, int32_t offset) {
	UV_ALLOC_CHECK(req, uv_fs_t);
	UV_ERROR_CHECK_C(uv_fs_read(loop, req, file, buf, 1, offset, NULL), free(req));
	return handle_fs_cb_int_sync(req);
}

HL_PRIM void HL_NAME(w_fs_write)(uv_loop_t *loop, uv_file file, const uv_buf_t *buf, int32_t offset, vclosure *cb) {
	UV_ALLOC_CHECK(req, uv_fs_t);
	UV_REQ_DATA(req) = (void *)cb;
	UV_ERROR_CHECK_C(uv_fs_write(loop, req, file, buf, 1, offset, handle_fs_cb_int), free(req));
	hl_add_root(UV_REQ_DATA(req));
}

HL_PRIM int HL_NAME(w_fs_write_sync)(uv_loop_t *loop, uv_file file, const uv_buf_t *buf, int32_t offset) {
	UV_ALLOC_CHECK(req, uv_fs_t);
	UV_ERROR_CHECK_C(uv_fs_write(loop, req, file, buf, 1, offset, NULL), free(req));
	return handle_fs_cb_int_sync(req);
}

*/
FS_WRAP1(fs_close, (uv_file), handle_fs_cb);
FS_WRAP3(fs_open, String_val, (int), (int), handle_fs_cb_file);
FS_WRAP1(fs_unlink, String_val, handle_fs_cb);
FS_WRAP2(fs_mkdir, String_val, (int), handle_fs_cb);
FS_WRAP1(fs_mkdtemp, String_val, handle_fs_cb_path);
FS_WRAP1(fs_rmdir, String_val, handle_fs_cb);
FS_WRAP2(fs_scandir, String_val, (int), handle_fs_cb_scandir);
//FS_WRAP1(fs_stat, vdynamic *, const char*, _STAT, _BYTES, _CB_STAT, handle_fs_cb_stat, return);
//FS_WRAP1(fs_fstat, vdynamic *, uv_file, _STAT, _FILE, _CB_STAT, handle_fs_cb_stat, return);
//FS_WRAP1(fs_lstat, vdynamic *, const char*, _STAT, _BYTES, _CB_STAT, handle_fs_cb_stat, return);
FS_WRAP2(fs_rename, String_val, String_val, handle_fs_cb);
FS_WRAP1(fs_fsync, (uv_file), handle_fs_cb);
FS_WRAP1(fs_fdatasync, (uv_file), handle_fs_cb);
FS_WRAP2(fs_ftruncate, (uv_file), Int64_val, handle_fs_cb);
//FS_WRAP4(fs_sendfile, void, uv_file, uv_file, int64_t, size_t, _VOID, _FILE _FILE _I32 _I32, _CB, handle_fs_cb, );
FS_WRAP2(fs_access, String_val, (int), handle_fs_cb);
FS_WRAP2(fs_chmod, String_val, (int), handle_fs_cb);
FS_WRAP2(fs_fchmod, (uv_file), (int), handle_fs_cb);
FS_WRAP3(fs_utime, String_val, Double_val, Double_val, handle_fs_cb);
FS_WRAP3(fs_futime, (uv_file), Double_val, Double_val, handle_fs_cb);
FS_WRAP2(fs_link, String_val, String_val, handle_fs_cb);
FS_WRAP3(fs_symlink, String_val, String_val, (int), handle_fs_cb);
FS_WRAP1(fs_readlink, String_val, handle_fs_cb_bytes);
FS_WRAP1(fs_realpath, String_val, handle_fs_cb_bytes);
FS_WRAP3(fs_chown, String_val, (uv_uid_t), (uv_gid_t), handle_fs_cb);
FS_WRAP3(fs_fchown, (uv_file), (uv_uid_t), (uv_gid_t), handle_fs_cb);
