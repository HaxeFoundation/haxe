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

package cpp.uv;

import haxe.io.Bytes;

using cpp.uv.UV;

typedef RUsage = {
	/** user CPU time used */
	var utime:{sec:Int64, usec:Int64};
	/** system CPU time used */
	var stime:{sec:Int64, usec:Int64};
	/** maximum resident set size */
	var maxrss:UInt64;
	/** integral shared memory size (X) */
	var ixrss:UInt64;
	/** integral unshared data size (X) */
	var idrss:UInt64;
	/** integral unshared stack size (X) */
	var isrss:UInt64;
	/** page reclaims (soft page faults) (X) */
	var minflt:UInt64;
	/** page faults (hard page faults) */
	var majflt:UInt64;
	/** swaps (X) */
	var nswap:UInt64;
	/** block input operations */
	var inblock:UInt64;
	/** block output operations */
	var oublock:UInt64;
	/** IPC messages sent (X) */
	var msgsnd:UInt64;
	/** IPC messages received (X) */
	var msgrcv:UInt64;
	/** signals received (X) */
	var nsignals:UInt64;
	/** voluntary context switches (X) */
	var nvcsw:UInt64;
	/** involuntary context switches (X) */
	var nivcsw:UInt64;
}

typedef CpuInfo = {
	var model:String;
	var speed:Int;
	var cpuTimes:{
		/** milliseconds */
		var user:UInt64;
		/** milliseconds */
		var nice:UInt64;
		/** milliseconds */
		var sys:UInt64;
		/** milliseconds */
		var idle:UInt64
		/** milliseconds */;
		var irq:UInt64;
	}
}

typedef InterfaceAddress = {
	var name:String;
	var physAddr:Array<Int>;
	var isInternal:Bool;
	var address:SockAddr;
	var netmask:SockAddr;
}

typedef Passwd = {
	var username:String;
	var uid:Int64;
	var gid:Int64;
	var homedir:String;
	var shell:Null<String>;
}

typedef Uname = {
	var sysname:String;
	var release:String;
	var version:String;
	var machine:String;
}

@:allow(cpp.uv)
@:headerCode('#include "uv.h"')
class RandomRequest extends Request {
	var uvRandom:RawPointer<UvRandomT>;
	var callback:(e:UVError)->Void;
	//to keep bytes alive while waiting for a callback
	var buf:Bytes;

	function setupUvReq() {
		uvRandom = UvRandomT.create();
		uvReq = cast uvRandom;
	}
}

/**
	Miscellaneous functions.

	@see http://docs.libuv.org/en/v1.x/misc.html
**/
@:headerCode('#include "uv.h"')
class Misc {

	/**
		Gets the resident set size (RSS) for the current process.
	**/
	static public function residentSetMemory():SizeT {
		var rss:SizeT = 0;
		UV.resident_set_memory(RawPointer.addressOf(rss)).resolve();
		return rss;
	}

	/**
		Gets the current system uptime.
	**/
	static public function uptime():Float {
		var uptime = 0.0;
		UV.uptime(RawPointer.addressOf(uptime)).resolve();
		return uptime;
	}

	/**
		Gets the resource usage measures for the current process.
	**/
	static public function getRUsage():RUsage {
		var rusage = new UvRusageT();
		UV.getrusage(RawPointer.addressOf(rusage)).resolve();
		return {
			utime: {sec:rusage.ru_utime.tv_sec, usec:rusage.ru_utime.tv_usec},
			stime: {sec:rusage.ru_stime.tv_sec, usec:rusage.ru_stime.tv_usec},
			maxrss: rusage.ru_maxrss,
			ixrss: rusage.ru_ixrss,
			idrss: rusage.ru_idrss,
			isrss: rusage.ru_isrss,
			minflt: rusage.ru_minflt,
			majflt: rusage.ru_majflt,
			nswap: rusage.ru_nswap,
			inblock: rusage.ru_inblock,
			oublock: rusage.ru_oublock,
			msgsnd: rusage.ru_msgsnd,
			msgrcv: rusage.ru_msgrcv,
			nsignals: rusage.ru_nsignals,
			nvcsw: rusage.ru_nvcsw,
			nivcsw: rusage.ru_nivcsw,
		}
	}

	/**
		Returns the current process ID.
	**/
	static public function getPid():Int {
		return UV.os_getpid();
	}

	/**
		Returns the parent process ID.
	**/
	static public function getPPid():Int {
		return UV.os_getppid();
	}

	/**
		Gets information about the CPUs on the system.
	**/
	static public function cpuInfo():Array<CpuInfo> {
		var infos:RawPointer<UvCpuInfoT> = null;
		var count = 0;
		UV.cpu_info(RawPointer.addressOf(infos), RawPointer.addressOf(count)).resolve();
		var ptr = Pointer.fromRaw(infos);
		var result = [for(i in 0...count) {
			var info = ptr.at(i);
			{
				model: info.model.charStarToString(),
				speed: info.speed,
				cpuTimes: {
					user: info.cpu_times.user,
					nice: info.cpu_times.nice,
					sys: info.cpu_times.sys,
					idle: info.cpu_times.idle,
					irq: info.cpu_times.irq,
				}
			}
		}];
		infos.free_cpu_info(count);
		return result;
	}

	/**
		Gets address information about the network interfaces on the system.
	**/
	static public function interfaceAddresses():Array<InterfaceAddress> {
		var addresses:RawPointer<UvInterfaceAddressT> = null;
		var count = 0;
		UV.interface_addresses(RawPointer.addressOf(addresses), RawPointer.addressOf(count)).resolve();
		var ptr = Pointer.fromRaw(addresses);
		var result = [for(i in 0...count) {
			var addr = ptr.at(i);
			var physAddr = Pointer.fromRaw(addr.phys_addr);
			var entry = {
				name:addr.name.charStarToString(),
				physAddr:[for(i in 0...6) (physAddr.at(i):Int)],
				isInternal:addr.is_internal != 0,
				address: new SockAddr(),
				netmask: new SockAddr(),
			}
			untyped __cpp__('memcpy({0}, {1}, sizeof(struct sockaddr_storage))', entry.address.storage, RawPointer.addressOf(addr.address));
			untyped __cpp__('memcpy({0}, {1}, sizeof(struct sockaddr_storage))', entry.netmask.storage, RawPointer.addressOf(addr.netmask));
			entry;
		}];
		UV.free_interface_addresses(addresses, count);
		return result;
	}

	/**
		Gets the temp directory.
	**/
	static public function loadAvg():Array<Float> {
		var averages:Pointer<Float> = untyped __cpp__('malloc(3 * sizeof(double))');
		UV.loadavg(averages.raw);
		var result = [for(i in 0...3) averages.at(i)];
		Stdlib.free(averages);
		return result;
	}

	/**
		Gets the current working directory.
	**/
	static public function cwd():String {
		return UV.getName((buf, size) -> UV.cwd(buf, size));
	}

	/**
		Changes the current working directory.
	**/
	static public function chdir(dir:String) {
		UV.chdir(dir).resolve();
	}

	/**
		Gets the current user’s home directory.
	**/
	static public function homeDir():String {
		return UV.getName((buf, size) -> UV.os_homedir(buf, size));
	}

	/**
		Gets the temp directory.
	**/
	static public function tmpDir():String {
		return UV.getName((buf, size) -> UV.os_tmpdir(buf, size));
	}

	/**
		Gets a subset of the password file entry for the current effective uid
		(not the real uid).
	**/
	static public function getPasswd():Passwd {
		var passwd = new UvPasswdT();
		UV.os_get_passwd(RawPointer.addressOf(passwd)).resolve();
		var result = {
			username: passwd.username.charStarToString(),
			uid: passwd.uid,
			gid: passwd.gid,
			shell: passwd.shell.charStarToString(),
			homedir: passwd.homedir.charStarToString(),
		}
		UV.os_free_passwd(RawPointer.addressOf(passwd));
		return result;
	}

	/**
		Gets the amount of free memory available in the system,
		as reported by the kernel (in bytes).
	**/
	static public function getFreeMemory():UInt64 {
		return UV.get_free_memory();
	}

	/**
		Gets the total amount of physical memory in the system (in bytes).
	**/
	static public function getTotalMemory():UInt64 {
		return UV.get_total_memory();
	}

	/**
		Gets the amount of memory available to the process (in bytes) based on limits imposed by the OS.
	**/
	static public function getConstrainedMemory():UInt64 {
		return UV.get_constrained_memory();
	}

	/**
		Returns the current high-resolution real time.
		This is expressed in nanoseconds.
		It is relative to an arbitrary time in the past. It is not related to the
		time of day and therefore not subject to clock drift. The primary use is
		for measuring performance between intervals.
	**/
	static public function hrTime():UInt64 {
		return UV.hrtime();
	}

	/**
		Returns the hostname.
	**/
	static public function getHostName():String {
		return UV.getName((buf, size) -> UV.os_gethostname(buf, size));
	}

	/**
		Retrieves the scheduling priority of the process specified by pid.
		The returned value of priority is between -20 (high priority) and 19 (low priority).
	**/
	static public function getPriority(pid:Int):Int {
		var p = 0;
		UV.os_getpriority(pid, RawPointer.addressOf(p)).resolve();
		return p;
	}

	/**
		Sets the scheduling priority of the process specified by pid.
		The priority value range is between -20 (high priority) and 19 (low priority).
	**/
	static public function setPriority(pid:Int, priority:Int):Void {
		UV.os_setpriority(pid, priority).resolve();
	}

	/**
		Retrieves system information.
	**/
	static public function uname():Uname @:privateAccess {
		var uname = new UvUtsnameT();
		UV.os_uname(RawPointer.addressOf(uname)).resolve();
		return {
			sysname: uname.sysname.charStarToString(),
			release: uname.release.charStarToString(),
			version: uname.version.charStarToString(),
			machine: uname.machine.charStarToString(),
		}
	}

	/**
		Get time.
	**/
	static public function getTimeOfDay():{sec:Int64, usec:Int} {
		var tv = new UvTimeval64T();
		UV.gettimeofday(RawPointer.addressOf(tv)).resolve();
		return {
			sec:tv.tv_sec,
			usec:tv.tv_usec,
		}
	}

	/**
		Fill `buf` starting at `pos` with exactly `length` cryptographically strong random bytes acquired
		from the system CSPRNG.
		`flags` is reserved for future extension and must currently be 0.
	**/
	static public function random(loop:Loop, buf:Bytes, pos:Int, length:Int, flags:Int, callback:(e:UVError)->Void):RandomRequest {
		var req = new RandomRequest();
		if(pos + length > buf.length)
			throw new UVException(UV_ENOBUFS);
		var base = NativeArray.getBase(buf.getData()).getBase();
		base = Pointer.addressOf(Pointer.fromRaw(base).at(pos)).raw;
		UV.random(loop.uvLoop, req.uvRandom, cast base, length, flags, Callable.fromStaticFunction(uvRandomCb)).resolve();
		req.buf = buf;
		req.callback = callback;
		return req;
	}

	static function uvRandomCb(uvRandom:RawPointer<UvRandomT>, status:Int, buf:RawPointer<cpp.Void>, buflen:SizeT):Void {
		var req:RandomRequest = cast Request.getRequest(cast uvRandom);
		req.callback(status.explain());
	}

	/**
		Synchronously fill `buf` starting at `pos` with exactly `length` cryptographically strong random bytes acquired
		from the system CSPRNG.
		`flags` is reserved for future extension and must currently be 0.
	**/
	static public function randomSync(buf:Bytes, pos:Int, length:Int, flags:Int):Void {
		if(pos + length > buf.length)
			throw new UVException(UV_ENOBUFS);
		var base = NativeArray.getBase(buf.getData()).getBase();
		base = Pointer.addressOf(Pointer.fromRaw(base).at(pos)).raw;
		UV.random(null, null, cast base, length, flags, null).resolve();
	}
}