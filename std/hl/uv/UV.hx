/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

// This file is automatically generated by a tool in Hashlink repo.
// see <hashlink_repo>/other/uvgenerator

// Contents of <hashlink_repo>/other/uvgenerator/UV.hx.header :

package hl.uv;

import hl.uv.Handle;
import hl.uv.Request;
import hl.uv.SockAddr;
import hl.uv.Dns;
import hl.uv.Dir;
import hl.uv.Loop;
import hl.uv.File;
import hl.uv.Tty;
import hl.uv.Udp;
import hl.uv.Process;
import hl.uv.Signal;

typedef UvUidT = Int;
typedef UvGidT = Int;
typedef UvPidT = Int;
typedef UvHandleType = HandleType;
typedef UvReqType = RequestType;
typedef UvRunMode = LoopRunMode;
typedef UvFsType = FsRequestType;
typedef UvTtyModeT = TtyMode;
typedef UvTtyVtermstateT = TtyVTermState;
typedef UvTtyVtermstateTStar = Ref<TtyVTermState>;
typedef UvMembership = UdpMembership;
typedef UvDirentTypeT = DirEntryType;

abstract UvFile(Int) {
	@:allow(hl.uv) inline function new(fd:Int) this = fd;
}

abstract UvUdpSendTStar(UvReqTStar) to UvReqTStar {}
abstract UvWriteTStar(UvReqTStar) to UvReqTStar {}
abstract UvShutdownTStar(UvReqTStar) to UvReqTStar {}
abstract UvRandomTStar(UvReqTStar) to UvReqTStar {}
abstract UvGetnameinfoTStar(UvReqTStar) to UvReqTStar {}
abstract UvGetaddrinfoTStar(UvReqTStar) to UvReqTStar {}
abstract UvFsTStar(UvReqTStar) to UvReqTStar {}
abstract UvConnectTStar(UvReqTStar) to UvReqTStar {}

abstract UvUdpTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvStreamTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvTtyTStar(UvStreamTStar) to UvStreamTStar to UvHandleTStar {}
abstract UvTcpTStar(UvStreamTStar) to UvStreamTStar to UvHandleTStar {}
abstract UvPipeTStar(UvStreamTStar) to UvStreamTStar to UvHandleTStar {}
abstract UvTimerTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvSignalTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvProcessTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvPrepareTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvIdleTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvFsPollTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvFsEventTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvCheckTStar(UvHandleTStar) to UvHandleTStar {}
abstract UvAsyncTStar(UvHandleTStar) to UvHandleTStar {}

abstract UvBufTArr(Abstract<"uv_buf_t_arr">) {}
abstract CSockaddrStorageStar(Abstract<"sockaddr_storage_star">) {}
abstract UvStdioContainerTStar(Abstract<"uv_stdio_container_t_star">) {}
abstract UvTimespecTStar(Abstract<"uv_timespec_t_star">) {}
abstract UvTimevalTStar(Abstract<"uv_timeval_t_star">) {}
abstract UvCpuTimesTStar(Abstract<"uv_cpu_times_t_star">) {}

//TODO: implement these
typedef UInt = Int;
typedef U64 = I64;

/**
	Automatically generated bindings for libuv.
	Avoid using this module directly.
	BACKWARD COMPATIBILITY OF THIS MODULE IS NOT MAINTAINED.
**/
@:hlNative("uv")
extern class UV {
	extern static public inline function resolve(result:Int):Int {
		if(result < 0)
			throw new UVException(translate_uv_error(result));
		return result;
	}

	extern static public inline function throwErr(result:Int):Int {
		throw new UVException(translate_uv_error(result));
	}

	extern static public inline function checkLoop(loop:Loop):Void {
		if(loop == null)
			throw new UVException(UV_EINVAL);
	}

	extern static public inline function toUTF8(s:String):Bytes {
		return @:privateAccess s.toUtf8();
	}

	extern static public inline function fromUTF8(b:Bytes):String {
		return @:privateAccess String.fromUTF8(b);
	}

	extern static public inline function getName(fn:(buf:Bytes, size:Ref<U64>)->Int):String {
		var size = I64.ofInt(256);
		var buf = null;
		var eNoBufs = UVError.UV_ENOBUFS.toNative();
		var result = eNoBufs;
		while (result == eNoBufs) {
			buf = new Bytes(size.toInt());
			result = fn(buf, Ref.make(size));
		}
		result.resolve();
		return buf.fromUTF8();
	}

	static public function alloc_char_array(length:Int):Ref<Bytes>;
	static public function free_char_array(a:Ref<Bytes>):Void;
	static public function free_bytes(bytes:Bytes):Void;
	static public function translate_uv_error(uvErrno:Int):UVError;
	static public function translate_to_uv_error(errno:Int):Int;
	static public function translate_sys_signal(sigNum:Int):SigNum;
	static public function translate_to_sys_signal(sigNum:SigNum):Int;
	static public function free_handle(data:UvHandleTStar):Void;
	static public function handle_set_data_with_gc<T:UvHandleTStar>(handle:UvHandleTStar, data:Handle<T>):Void;
	static public function req_set_data_with_gc<T:UvReqTStar>(req:UvReqTStar, data:Request<T>):Void;
	static public function free_req(req:UvReqTStar):Void;
	static public function alloc_loop():UvLoopTStar;
	static public function free_loop(req:UvLoopTStar):Void;
	static public function alloc_async():UvAsyncTStar;
	static public function alloc_timer():UvTimerTStar;
	static public function alloc_check():UvCheckTStar;
	static public function alloc_prepare():UvPrepareTStar;
	static public function alloc_idle():UvIdleTStar;
	static public function alloc_tcp():UvTcpTStar;
	static public function alloc_sockaddr_storage():CSockaddrStorageStar;
	static public function sockaddr_storage_size():Int;
	static public function free_sockaddr_storage(addr:CSockaddrStorageStar):Void;
	static public function sockaddr_of_storage(addr:CSockaddrStorageStar):CSockaddrStar;
	static public function sockaddr_to_storage(addr:CSockaddrStar):CSockaddrStorageStar;
	static public function alloc_udp():UvUdpTStar;
	static public function alloc_udp_send():UvUdpSendTStar;
	static public function alloc_pipe():UvPipeTStar;
	static public function alloc_stdio_container(stdio:NativeArray<Dynamic>, count:Int):UvStdioContainerTStar; //Dynamic should contain hl.uv.Process.ProcessStdio instances
	static public function free_stdio_container(stdio:UvStdioContainerTStar):Void;
	static public function alloc_process_options(file:Bytes, args:Ref<Bytes>, env:Ref<Bytes>, cwd:Bytes, flags:Int, stdio_count:Int, stdio:UvStdioContainerTStar, uid:UvUidT, gid:UvGidT):UvProcessOptionsTStar;
	static public function free_process_options(options:UvProcessOptionsTStar):Void;
	static public function alloc_process():UvProcessTStar;
	static public function alloc_getaddrinfo():UvGetaddrinfoTStar;
	static public function alloc_getnameinfo():UvGetnameinfoTStar;
	static public function alloc_addrinfo(flags:Int, family:AddressFamily, socktype:SocketType, protocol:Int):CAddrinfoStar;
	static public function alloc_shutdown():UvShutdownTStar;
	static public function alloc_write():UvWriteTStar;
	static public function alloc_connect():UvConnectTStar;
	static public function address_family_to_af(family:AddressFamily):Int;
	static public function address_family_to_pf(family:AddressFamily):Int;
	static public function addrinfo_ai_family(ai:CAddrinfoStar):AddressFamily;
	static public function addrinfo_ai_socktype(ai:CAddrinfoStar):SocketType;
	static public function addrinfo_ai_protocol(ai:CAddrinfoStar):Int;
	static public function addrinfo_ai_addr(ai:CAddrinfoStar):SockAddr;
	static public function addrinfo_ai_canonname(ai:CAddrinfoStar):Bytes;
	static public function addrinfo_ai_next(ai:CAddrinfoStar):Null<CAddrinfoStar>;
	static public function nameinfo_flags_to_native(ai:NameInfoFlags):Int;
	static public function alloc_fs():UvFsTStar;
	static public function alloc_fs_event():UvFsEventTStar;
	static public function alloc_fs_poll():UvFsPollTStar;
	static public function pointer_to_dir(req:Pointer):UvDirTStar;
	static public function free_dir(dir:UvDirTStar):Void;
	static public function dir_init(dir:UvDirTStar, num_entries:Int):Void;
	static public function dir_nentries(dir:UvDirTStar):Int;
	static public function dir_dirent(dir:UvDirTStar, index:Int):UvDirentTStar;
	static public function free_dirent(dirent:UvDirentTStar):Void;
	static public function free_buf(buf:UvBufTArr):Void;
	static public function alloc_buf(bytes:Bytes, bytesLength:Int):UvBufTArr;
	static public function buf_set(buf:UvBufTArr, base:Bytes, length:Int):Void;
	static public function buf_base(buf:UvBufTArr):Bytes;
	static public function buf_len(buf:UvBufTArr):U64;
	static public function alloc_tty():UvTtyTStar;
	static public function alloc_signal():UvSignalTStar;
	static public function signal_signum(signal:UvSignalTStar):Int;
	static public function version_major():Int;
	static public function version_minor():Int;
	static public function version_patch():Int;
	static public function version_hex():Int;
	static public function version_suffix():Bytes;
	static public function version_is_release():Bool;
	static public function alloc_cpu_info():UvCpuInfoTStar;
	static public function cpu_info_model(cpu_info:UvCpuInfoTStar):Bytes;
	static public function cpu_info_speed(cpu_info:UvCpuInfoTStar):Int;
	static public function cpu_info_cpu_times(cpu_info:UvCpuInfoTStar):UvCpuTimesTStar;
	static public function cpu_times_user(cpu_times:UvCpuTimesTStar):U64;
	static public function cpu_times_nice(cpu_times:UvCpuTimesTStar):U64;
	static public function cpu_times_sys(cpu_times:UvCpuTimesTStar):U64;
	static public function cpu_times_idle(cpu_times:UvCpuTimesTStar):U64;
	static public function cpu_times_irq(cpu_times:UvCpuTimesTStar):U64;

// Auto generated content :

	static public function async_init_with_cb(loop:UvLoopTStar, async:UvAsyncTStar):Int;
	static public function async_send(async:UvAsyncTStar):Int;
	static public function check_init(loop:UvLoopTStar, check:UvCheckTStar):Int;
	static public function check_start_with_cb(check:UvCheckTStar):Int;
	static public function check_stop(check:UvCheckTStar):Int;
	static public function getaddrinfo_with_cb(loop:UvLoopTStar, req:UvGetaddrinfoTStar, node:Bytes, service:Bytes, hints:CAddrinfoStar):Int;
	static public function freeaddrinfo(ai:CAddrinfoStar):Void;
	static public function getnameinfo_with_cb(loop:UvLoopTStar, req:UvGetnameinfoTStar, addr:CSockaddrStar, flags:Int):Int;
	static public function strerror(err:Int):Bytes;
	static public function strerror_r(err:Int, buf:Bytes, buflen:U64):Bytes;
	static public function err_name(err:Int):Bytes;
	static public function err_name_r(err:Int, buf:Bytes, buflen:U64):Bytes;
	static public function translate_sys_error(sys_errno:Int):Int;
	static public function timespec_tv_sec(timespec:UvTimespecTStar):I64;
	static public function timespec_tv_nsec(timespec:UvTimespecTStar):I64;
	static public function alloc_timespec():UvTimespecTStar;
	static public function stat_st_dev(stat:UvStatTStar):U64;
	static public function stat_st_mode(stat:UvStatTStar):U64;
	static public function stat_st_nlink(stat:UvStatTStar):U64;
	static public function stat_st_uid(stat:UvStatTStar):U64;
	static public function stat_st_gid(stat:UvStatTStar):U64;
	static public function stat_st_rdev(stat:UvStatTStar):U64;
	static public function stat_st_ino(stat:UvStatTStar):U64;
	static public function stat_st_size(stat:UvStatTStar):U64;
	static public function stat_st_blksize(stat:UvStatTStar):U64;
	static public function stat_st_blocks(stat:UvStatTStar):U64;
	static public function stat_st_flags(stat:UvStatTStar):U64;
	static public function stat_st_gen(stat:UvStatTStar):U64;
	static public function stat_st_atim(stat:UvStatTStar):UvTimespecTStar;
	static public function stat_st_mtim(stat:UvStatTStar):UvTimespecTStar;
	static public function stat_st_ctim(stat:UvStatTStar):UvTimespecTStar;
	static public function stat_st_birthtim(stat:UvStatTStar):UvTimespecTStar;
	static public function alloc_stat():UvStatTStar;
	static public function statfs_f_type(statfs:UvStatfsTStar):U64;
	static public function statfs_f_bsize(statfs:UvStatfsTStar):U64;
	static public function statfs_f_blocks(statfs:UvStatfsTStar):U64;
	static public function statfs_f_bfree(statfs:UvStatfsTStar):U64;
	static public function statfs_f_bavail(statfs:UvStatfsTStar):U64;
	static public function statfs_f_files(statfs:UvStatfsTStar):U64;
	static public function statfs_f_ffree(statfs:UvStatfsTStar):U64;
	static public function alloc_statfs():UvStatfsTStar;
	static public function dirent_name(dirent:UvDirentTStar):Bytes;
	static public function dirent_type(dirent:UvDirentTStar):Int;
	static public function alloc_dirent():UvDirentTStar;
	static public function fs_req_cleanup(req:UvFsTStar):Void;
	static public function fs_close_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, use_uv_fs_cb:Bool):Int;
	static public function fs_open_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, flags:Int, mode:Int, use_uv_fs_cb:Bool):Int;
	static public function fs_read_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, bufs:UvBufTArr, nbufs:UInt, offset:I64, use_uv_fs_cb:Bool):Int;
	static public function fs_unlink_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_write_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, bufs:UvBufTArr, nbufs:UInt, offset:I64, use_uv_fs_cb:Bool):Int;
	static public function fs_mkdir_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, mode:Int, use_uv_fs_cb:Bool):Int;
	static public function fs_mkdtemp_with_cb(loop:UvLoopTStar, req:UvFsTStar, tpl:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_mkstemp_with_cb(loop:UvLoopTStar, req:UvFsTStar, tpl:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_rmdir_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_opendir_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_closedir_with_cb(loop:UvLoopTStar, req:UvFsTStar, dir:UvDirTStar, use_uv_fs_cb:Bool):Int;
	static public function fs_readdir_with_cb(loop:UvLoopTStar, req:UvFsTStar, dir:UvDirTStar, use_uv_fs_cb:Bool):Int;
	static public function fs_scandir_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, flags:Int, use_uv_fs_cb:Bool):Int;
	static public function fs_scandir_next(req:UvFsTStar, ent:UvDirentTStar):Int;
	static public function fs_stat_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_fstat_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, use_uv_fs_cb:Bool):Int;
	static public function fs_lstat_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_statfs_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_rename_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, new_path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_fsync_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, use_uv_fs_cb:Bool):Int;
	static public function fs_fdatasync_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, use_uv_fs_cb:Bool):Int;
	static public function fs_ftruncate_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, offset:I64, use_uv_fs_cb:Bool):Int;
	static public function fs_copyfile_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, new_path:Bytes, flags:Int, use_uv_fs_cb:Bool):Int;
	static public function fs_sendfile_with_cb(loop:UvLoopTStar, req:UvFsTStar, out_fd:UvFile, in_fd:UvFile, in_offset:I64, length:U64, use_uv_fs_cb:Bool):Int;
	static public function fs_access_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, mode:Int, use_uv_fs_cb:Bool):Int;
	static public function fs_chmod_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, mode:Int, use_uv_fs_cb:Bool):Int;
	static public function fs_fchmod_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, mode:Int, use_uv_fs_cb:Bool):Int;
	static public function fs_utime_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, atime:Float, mtime:Float, use_uv_fs_cb:Bool):Int;
	static public function fs_futime_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, atime:Float, mtime:Float, use_uv_fs_cb:Bool):Int;
	static public function fs_lutime_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, atime:Float, mtime:Float, use_uv_fs_cb:Bool):Int;
	static public function fs_link_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, new_path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_symlink_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, new_path:Bytes, flags:Int, use_uv_fs_cb:Bool):Int;
	static public function fs_readlink_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_realpath_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, use_uv_fs_cb:Bool):Int;
	static public function fs_chown_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, uid:UvUidT, gid:UvGidT, use_uv_fs_cb:Bool):Int;
	static public function fs_fchown_with_cb(loop:UvLoopTStar, req:UvFsTStar, file:UvFile, uid:UvUidT, gid:UvGidT, use_uv_fs_cb:Bool):Int;
	static public function fs_lchown_with_cb(loop:UvLoopTStar, req:UvFsTStar, path:Bytes, uid:UvUidT, gid:UvGidT, use_uv_fs_cb:Bool):Int;
	static public function fs_get_type(req:UvFsTStar):UvFsType;
	static public function fs_get_result(req:UvFsTStar):I64;
	static public function fs_get_system_error(req:UvFsTStar):Int;
	static public function fs_get_ptr(req:UvFsTStar):Pointer;
	static public function fs_get_path(req:UvFsTStar):Bytes;
	static public function fs_get_statbuf(req:UvFsTStar):UvStatTStar;
	static public function fs_event_init(loop:UvLoopTStar, handle:UvFsEventTStar):Int;
	static public function fs_event_start_with_cb(handle:UvFsEventTStar, path:Bytes, flags:UInt):Int;
	static public function fs_event_stop(handle:UvFsEventTStar):Int;
	static public function fs_event_getpath(handle:UvFsEventTStar, buffer:Bytes, size:Ref<U64>):Int;
	static public function fs_poll_init(loop:UvLoopTStar, handle:UvFsPollTStar):Int;
	static public function fs_poll_start_with_cb(handle:UvFsPollTStar, path:Bytes, interval:UInt):Int;
	static public function fs_poll_stop(handle:UvFsPollTStar):Int;
	static public function fs_poll_getpath(handle:UvFsPollTStar, buffer:Bytes, size:Ref<U64>):Int;
	static public function is_active(handle:UvHandleTStar):Int;
	static public function is_closing(handle:UvHandleTStar):Int;
	static public function close_with_cb(handle:UvHandleTStar):Void;
	static public function ref(handle:UvHandleTStar):Void;
	static public function unref(handle:UvHandleTStar):Void;
	static public function has_ref(handle:UvHandleTStar):Int;
	static public function handle_size(type:UvHandleType):U64;
	static public function send_buffer_size(handle:UvHandleTStar, value:Ref<Int>):Int;
	static public function recv_buffer_size(handle:UvHandleTStar, value:Ref<Int>):Int;
	static public function handle_get_loop(handle:UvHandleTStar):UvLoopTStar;
	static public function handle_get_data(handle:UvHandleTStar):Pointer;
	static public function handle_set_data(handle:UvHandleTStar, data:Pointer):Pointer;
	static public function handle_get_type(handle:UvHandleTStar):UvHandleType;
	static public function handle_type_name(type:UvHandleType):Bytes;
	static public function idle_init(loop:UvLoopTStar, idle:UvIdleTStar):Int;
	static public function idle_start_with_cb(idle:UvIdleTStar):Int;
	static public function idle_stop(idle:UvIdleTStar):Int;
	static public function loop_init(loop:UvLoopTStar):Int;
	static public function loop_close(loop:UvLoopTStar):Int;
	static public function default_loop():UvLoopTStar;
	static public function run(loop:UvLoopTStar, mode:UvRunMode):Int;
	static public function loop_alive(loop:UvLoopTStar):Int;
	static public function stop(loop:UvLoopTStar):Void;
	static public function loop_size():U64;
	static public function backend_fd(loop:UvLoopTStar):Int;
	static public function backend_timeout(loop:UvLoopTStar):Int;
	static public function now(loop:UvLoopTStar):U64;
	static public function update_time(loop:UvLoopTStar):Void;
	static public function walk_with_cb(loop:UvLoopTStar, arg:Pointer):Void;
	static public function loop_fork(loop:UvLoopTStar):Int;
	static public function loop_get_data(loop:UvLoopTStar):Pointer;
	static public function loop_set_data(loop:UvLoopTStar, data:Pointer):Pointer;
	static public function metrics_idle_time(loop:UvLoopTStar):U64;
	static public function timeval_tv_sec(timeval:UvTimevalTStar):I64;
	static public function timeval_tv_usec(timeval:UvTimevalTStar):I64;
	static public function alloc_timeval():UvTimevalTStar;
	static public function timeval64_tv_sec(timeval64:UvTimeval64TStar):I64;
	static public function timeval64_tv_usec(timeval64:UvTimeval64TStar):Int;
	static public function alloc_timeval64():UvTimeval64TStar;
	static public function rusage_ru_utime(rusage:UvRusageTStar):UvTimevalTStar;
	static public function rusage_ru_stime(rusage:UvRusageTStar):UvTimevalTStar;
	static public function rusage_ru_maxrss(rusage:UvRusageTStar):U64;
	static public function rusage_ru_ixrss(rusage:UvRusageTStar):U64;
	static public function rusage_ru_idrss(rusage:UvRusageTStar):U64;
	static public function rusage_ru_isrss(rusage:UvRusageTStar):U64;
	static public function rusage_ru_minflt(rusage:UvRusageTStar):U64;
	static public function rusage_ru_majflt(rusage:UvRusageTStar):U64;
	static public function rusage_ru_nswap(rusage:UvRusageTStar):U64;
	static public function rusage_ru_inblock(rusage:UvRusageTStar):U64;
	static public function rusage_ru_oublock(rusage:UvRusageTStar):U64;
	static public function rusage_ru_msgsnd(rusage:UvRusageTStar):U64;
	static public function rusage_ru_msgrcv(rusage:UvRusageTStar):U64;
	static public function rusage_ru_nsignals(rusage:UvRusageTStar):U64;
	static public function rusage_ru_nvcsw(rusage:UvRusageTStar):U64;
	static public function rusage_ru_nivcsw(rusage:UvRusageTStar):U64;
	static public function alloc_rusage():UvRusageTStar;
	static public function passwd_username(passwd:UvPasswdTStar):Bytes;
	static public function passwd_uid(passwd:UvPasswdTStar):I64;
	static public function passwd_gid(passwd:UvPasswdTStar):I64;
	static public function passwd_shell(passwd:UvPasswdTStar):Bytes;
	static public function passwd_homedir(passwd:UvPasswdTStar):Bytes;
	static public function alloc_passwd():UvPasswdTStar;
	static public function guess_handle(file:UvFile):UvHandleType;
	static public function library_shutdown():Void;
	static public function resident_set_memory(rss:Ref<U64>):Int;
	static public function uptime(uptime:Ref<Float>):Int;
	static public function getrusage(rusage:UvRusageTStar):Int;
	static public function os_getpid():UvPidT;
	static public function os_getppid():UvPidT;
	static public function cpu_info(cpu_infos:Ref<UvCpuInfoTStar>, count:Ref<Int>):Int;
	static public function free_cpu_info(cpu_infos:UvCpuInfoTStar, count:Int):Void;
	static public function interface_addresses(addresses:Ref<UvInterfaceAddressTStar>, count:Ref<Int>):Int;
	static public function free_interface_addresses(addresses:UvInterfaceAddressTStar, count:Int):Void;
	static public function loadavg(avg:Ref<Float>):Void;
	static public function ip4_addr(ip:Bytes, port:Int, addr:CSockaddrInStar):Int;
	static public function ip6_addr(ip:Bytes, port:Int, addr:CSockaddrIn6Star):Int;
	static public function ip4_name(src:CSockaddrInStar, dst:Bytes, size:U64):Int;
	static public function ip6_name(src:CSockaddrIn6Star, dst:Bytes, size:U64):Int;
	static public function if_indextoname(ifindex:UInt, buffer:Bytes, size:Ref<U64>):Int;
	static public function if_indextoiid(ifindex:UInt, buffer:Bytes, size:Ref<U64>):Int;
	static public function exepath(buffer:Bytes, size:Ref<U64>):Int;
	static public function cwd(buffer:Bytes, size:Ref<U64>):Int;
	static public function chdir(dir:Bytes):Int;
	static public function os_homedir(buffer:Bytes, size:Ref<U64>):Int;
	static public function os_tmpdir(buffer:Bytes, size:Ref<U64>):Int;
	static public function os_get_passwd(pwd:UvPasswdTStar):Int;
	static public function os_free_passwd(pwd:UvPasswdTStar):Void;
	static public function get_free_memory():U64;
	static public function get_total_memory():U64;
	static public function get_constrained_memory():U64;
	static public function hrtime():U64;
	static public function os_getenv(name:Bytes, buffer:Bytes, size:Ref<U64>):Int;
	static public function os_setenv(name:Bytes, value:Bytes):Int;
	static public function os_unsetenv(name:Bytes):Int;
	static public function os_gethostname(buffer:Bytes, size:Ref<U64>):Int;
	static public function os_getpriority(pid:UvPidT, priority:Ref<Int>):Int;
	static public function os_setpriority(pid:UvPidT, priority:Int):Int;
	static public function os_uname(buffer:UvUtsnameTStar):Int;
	static public function gettimeofday(tv:UvTimeval64TStar):Int;
	static public function random_with_cb(loop:UvLoopTStar, req:UvRandomTStar, buf:Pointer, buflen:U64, flags:UInt):Int;
	static public function sleep(msec:UInt):Void;
	static public function pipe_init(loop:UvLoopTStar, handle:UvPipeTStar, ipc:Int):Int;
	static public function pipe_open(handle:UvPipeTStar, file:UvFile):Int;
	static public function pipe_bind(handle:UvPipeTStar, name:Bytes):Int;
	static public function pipe_connect_with_cb(req:UvConnectTStar, handle:UvPipeTStar, name:Bytes):Void;
	static public function pipe_getsockname(handle:UvPipeTStar, buffer:Bytes, size:Ref<U64>):Int;
	static public function pipe_getpeername(handle:UvPipeTStar, buffer:Bytes, size:Ref<U64>):Int;
	static public function pipe_pending_instances(handle:UvPipeTStar, count:Int):Void;
	static public function pipe_pending_count(handle:UvPipeTStar):Int;
	static public function pipe_pending_type(handle:UvPipeTStar):UvHandleType;
	static public function pipe_chmod(handle:UvPipeTStar, flags:Int):Int;
	static public function pipe(fds:Ref<UvFile>, read_flags:Int, write_flags:Int):Int;
	static public function prepare_init(loop:UvLoopTStar, prepare:UvPrepareTStar):Int;
	static public function prepare_start_with_cb(prepare:UvPrepareTStar):Int;
	static public function prepare_stop(prepare:UvPrepareTStar):Int;
	static public function disable_stdio_inheritance():Void;
	static public function spawn(loop:UvLoopTStar, handle:UvProcessTStar, options:UvProcessOptionsTStar):Int;
	static public function process_kill(handle:UvProcessTStar, signum:Int):Int;
	static public function kill(pid:Int, signum:Int):Int;
	static public function process_get_pid(handle:UvProcessTStar):UvPidT;
	static public function cancel(req:UvReqTStar):Int;
	static public function req_size(type:UvReqType):U64;
	static public function req_get_data(req:UvReqTStar):Pointer;
	static public function req_set_data(req:UvReqTStar, data:Pointer):Pointer;
	static public function req_get_type(req:UvReqTStar):UvReqType;
	static public function req_type_name(type:UvReqType):Bytes;
	static public function signal_init(loop:UvLoopTStar, signal:UvSignalTStar):Int;
	static public function signal_start_with_cb(signal:UvSignalTStar, signum:Int):Int;
	static public function signal_start_oneshot_with_cb(signal:UvSignalTStar, signum:Int):Int;
	static public function signal_stop(signal:UvSignalTStar):Int;
	static public function shutdown_with_cb(req:UvShutdownTStar, handle:UvStreamTStar):Int;
	static public function listen_with_cb(stream:UvStreamTStar, backlog:Int):Int;
	static public function accept(server:UvStreamTStar, client:UvStreamTStar):Int;
	static public function read_start_with_cb(stream:UvStreamTStar):Int;
	static public function read_stop(uvstreamtstar:UvStreamTStar):Int;
	static public function write_with_cb(req:UvWriteTStar, handle:UvStreamTStar, bufs:UvBufTArr, nbufs:UInt):Int;
	static public function write2_with_cb(req:UvWriteTStar, handle:UvStreamTStar, bufs:UvBufTArr, nbufs:UInt, send_handle:UvStreamTStar):Int;
	static public function try_write(handle:UvStreamTStar, bufs:UvBufTArr, nbufs:UInt):Int;
	static public function try_write2(handle:UvStreamTStar, bufs:UvBufTArr, nbufs:UInt, send_handle:UvStreamTStar):Int;
	static public function is_readable(handle:UvStreamTStar):Int;
	static public function is_writable(handle:UvStreamTStar):Int;
	static public function stream_set_blocking(handle:UvStreamTStar, blocking:Int):Int;
	static public function stream_get_write_queue_size(stream:UvStreamTStar):U64;
	static public function tcp_init(loop:UvLoopTStar, handle:UvTcpTStar):Int;
	static public function tcp_init_ex(loop:UvLoopTStar, handle:UvTcpTStar, flags:UInt):Int;
	static public function tcp_nodelay(handle:UvTcpTStar, enable:Int):Int;
	static public function tcp_keepalive(handle:UvTcpTStar, enable:Int, delay:UInt):Int;
	static public function tcp_simultaneous_accepts(handle:UvTcpTStar, enable:Int):Int;
	static public function tcp_bind(handle:UvTcpTStar, addr:CSockaddrStar, flags:UInt):Int;
	static public function tcp_getsockname(handle:UvTcpTStar, name:CSockaddrStar, namelen:Ref<Int>):Int;
	static public function tcp_getpeername(handle:UvTcpTStar, name:CSockaddrStar, namelen:Ref<Int>):Int;
	static public function tcp_connect_with_cb(req:UvConnectTStar, handle:UvTcpTStar, addr:CSockaddrStar):Int;
	static public function tcp_close_reset_with_cb(handle:UvTcpTStar):Int;
	static public function timer_init(loop:UvLoopTStar, handle:UvTimerTStar):Int;
	static public function timer_start_with_cb(handle:UvTimerTStar, timeout:U64, repeat:U64):Int;
	static public function timer_stop(handle:UvTimerTStar):Int;
	static public function timer_again(handle:UvTimerTStar):Int;
	static public function timer_set_repeat(handle:UvTimerTStar, repeat:U64):Void;
	static public function timer_get_repeat(handle:UvTimerTStar):U64;
	static public function timer_get_due_in(handle:UvTimerTStar):U64;
	static public function tty_init(loop:UvLoopTStar, handle:UvTtyTStar, fd:UvFile, unused:Int):Int;
	static public function tty_set_mode(handle:UvTtyTStar, mode:UvTtyModeT):Int;
	static public function tty_reset_mode():Int;
	static public function tty_get_winsize(handle:UvTtyTStar, width:Ref<Int>, height:Ref<Int>):Int;
	static public function tty_set_vterm_state(state:UvTtyVtermstateT):Void;
	static public function tty_get_vterm_state(state:UvTtyVtermstateTStar):Int;
	static public function udp_init(loop:UvLoopTStar, handle:UvUdpTStar):Int;
	static public function udp_init_ex(loop:UvLoopTStar, handle:UvUdpTStar, flags:UInt):Int;
	static public function udp_bind(handle:UvUdpTStar, addr:CSockaddrStar, flags:UInt):Int;
	static public function udp_connect(handle:UvUdpTStar, addr:CSockaddrStar):Int;
	static public function udp_getpeername(handle:UvUdpTStar, name:CSockaddrStar, namelen:Ref<Int>):Int;
	static public function udp_getsockname(handle:UvUdpTStar, name:CSockaddrStar, namelen:Ref<Int>):Int;
	static public function udp_set_membership(handle:UvUdpTStar, multicast_addr:Bytes, interface_addr:Bytes, membership:UvMembership):Int;
	static public function udp_set_source_membership(handle:UvUdpTStar, multicast_addr:Bytes, interface_addr:Bytes, source_addr:Bytes, membership:UvMembership):Int;
	static public function udp_set_multicast_loop(handle:UvUdpTStar, on:Int):Int;
	static public function udp_set_multicast_ttl(handle:UvUdpTStar, ttl:Int):Int;
	static public function udp_set_multicast_interface(handle:UvUdpTStar, interface_addr:Bytes):Int;
	static public function udp_set_broadcast(handle:UvUdpTStar, on:Int):Int;
	static public function udp_set_ttl(handle:UvUdpTStar, ttl:Int):Int;
	static public function udp_send_with_cb(req:UvUdpSendTStar, handle:UvUdpTStar, bufs:UvBufTArr, nbufs:UInt, addr:CSockaddrStar):Int;
	static public function udp_try_send(handle:UvUdpTStar, bufs:UvBufTArr, nbufs:UInt, addr:CSockaddrStar):Int;
	static public function udp_recv_start_with_cb(handle:UvUdpTStar):Int;
	static public function udp_using_recvmmsg(handle:UvUdpTStar):Int;
	static public function udp_recv_stop(handle:UvUdpTStar):Int;
	static public function udp_get_send_queue_size(handle:UvUdpTStar):U64;
	static public function udp_get_send_queue_count(handle:UvUdpTStar):U64;
	static public function version():UInt;
	static public function version_string():Bytes;
}

abstract UvUtsnameTStar(Abstract<"uv_utsname_t_star">) {}
abstract UvTimeval64TStar(Abstract<"uv_timeval64_t_star">) {}
abstract UvStatfsTStar(Abstract<"uv_statfs_t_star">) {}
abstract UvStatTStar(Abstract<"uv_stat_t_star">) {}
abstract UvRusageTStar(Abstract<"uv_rusage_t_star">) {}
abstract UvReqTStar(Abstract<"uv_req_t_star">) {}
abstract UvProcessOptionsTStar(Abstract<"uv_process_options_t_star">) {}
abstract UvPasswdTStar(Abstract<"uv_passwd_t_star">) {}
abstract UvLoopTStar(Abstract<"uv_loop_t_star">) {}
abstract UvInterfaceAddressTStar(Abstract<"uv_interface_address_t_star">) {}
abstract UvHandleTStar(Abstract<"uv_handle_t_star">) {}
abstract UvDirentTStar(Abstract<"uv_dirent_t_star">) {}
abstract UvDirTStar(Abstract<"uv_dir_t_star">) {}
abstract UvCpuInfoTStar(Abstract<"uv_cpu_info_t_star">) {}
abstract UvBufT(Abstract<"uv_buf_t">) {}
abstract CSockaddrStar(Abstract<"sockaddr_star">) {}
abstract CSockaddrInStar(Abstract<"sockaddr_in_star">) {}
abstract CSockaddrIn6Star(Abstract<"sockaddr_in6_star">) {}
abstract CAddrinfoStar(Abstract<"addrinfo_star">) {}