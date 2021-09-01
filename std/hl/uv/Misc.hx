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

package hl.uv;

import hl.uv.Request;

typedef RUsage = {
	/** user CPU time used */
	var utime:{sec:I64, usec:I64};
	/** system CPU time used */
	var stime:{sec:I64, usec:I64};
	/** maximum resident set size */
	var maxrss:U64;
	/** integral shared memory size (X) */
	var ixrss:U64;
	/** integral unshared data size (X) */
	var idrss:U64;
	/** integral unshared stack size (X) */
	var isrss:U64;
	/** page reclaims (soft page faults) (X) */
	var minflt:U64;
	/** page faults (hard page faults) */
	var majflt:U64;
	/** swaps (X) */
	var nswap:U64;
	/** block input operations */
	var inblock:U64;
	/** block output operations */
	var oublock:U64;
	/** IPC messages sent (X) */
	var msgsnd:U64;
	/** IPC messages received (X) */
	var msgrcv:U64;
	/** signals received (X) */
	var nsignals:U64;
	/** voluntary context switches (X) */
	var nvcsw:U64;
	/** involuntary context switches (X) */
	var nivcsw:U64;
}

typedef CpuInfo = {
	var model:String;
	var speed:Int;
	var cpuTimes:{
		/** milliseconds */
		var user:U64;
		/** milliseconds */
		var nice:U64;
		/** milliseconds */
		var sys:U64;
		/** milliseconds */
		var idle:U64
		/** milliseconds */;
		var irq:U64;
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
	var uid:I64;
	var gid:I64;
	var homedir:String;
	var shell:Null<String>;
}

typedef Uname = {
	var sysname:String;
	var release:String;
	var version:String;
	var machine:String;
}

@:allow(hl.uv)
class RandomRequest extends Request<UvRandomTStar> {
	@:keep var callback:(status:Int)->Void;
}

/**
	Miscellaneous.

	@see http://docs.libuv.org/en/v1.x/misc.html
**/
class Misc {
	/**
		Gets the resident set size (RSS) for the current process.
	**/
	static public function residentSetMemory():I64 {
		var rss = I64.ofInt(0);
		UV.resident_set_memory(Ref.make(rss)).resolve();
		return rss;
	}

	/**
		Gets the current system uptime.
	**/
	static public function uptime():Float {
		var uptime = 0.0;
		UV.uptime(Ref.make(uptime)).resolve();
		return uptime;
	}

	/**
		Gets the resource usage measures for the current process.
	**/
	static public function getRUsage():RUsage {
		var rusage = UV.alloc_rusage();
		var result = UV.getrusage(rusage);
		if(result < 0) {
			rusage.free_rusage();
			result.throwErr();
		}
		var utime = rusage.rusage_ru_utime();
		var stime = rusage.rusage_ru_stime();
		var result = {
			utime: {sec:utime.timeval_tv_sec(), usec:utime.timeval_tv_usec()},
			stime: {sec:stime.timeval_tv_sec(), usec:stime.timeval_tv_usec()},
			maxrss: rusage.rusage_ru_maxrss(),
			ixrss: rusage.rusage_ru_ixrss(),
			idrss: rusage.rusage_ru_idrss(),
			isrss: rusage.rusage_ru_isrss(),
			minflt: rusage.rusage_ru_minflt(),
			majflt: rusage.rusage_ru_majflt(),
			nswap: rusage.rusage_ru_nswap(),
			inblock: rusage.rusage_ru_inblock(),
			oublock: rusage.rusage_ru_oublock(),
			msgsnd: rusage.rusage_ru_msgsnd(),
			msgrcv: rusage.rusage_ru_msgrcv(),
			nsignals: rusage.rusage_ru_nsignals(),
			nvcsw: rusage.rusage_ru_nvcsw(),
			nivcsw: rusage.rusage_ru_nivcsw(),
		}
		rusage.free_rusage();
		return result;
	}

	/**
		Returns the current process ID.
	**/
	static public inline function getPid():Int {
		return UV.os_getpid();
	}

	/**
		Returns the parent process ID.
	**/
	static public inline function getPPid():Int {
		return UV.os_getppid();
	}

	/**
		Gets information about the CPUs on the system.
	**/
	static public function cpuInfo():Array<CpuInfo> {
		var infos = null;
		var count = 0;
		var result = UV.cpu_info(Ref.make(infos), Ref.make(count));
		if(result < 0) {
			infos.free_cpu_info(count);
			result.throwErr();
		}
		var result = [for(i in 0...count) {
			var info = infos.cpu_info_get(i);
			var times = info.cpu_info_cpu_times();
			{
				model: info.cpu_info_model().fromUTF8(),
				speed: info.cpu_info_speed(),
				cpuTimes: {
					user: times.cpu_times_user(),
					nice: times.cpu_times_nice(),
					sys: times.cpu_times_sys(),
					idle: times.cpu_times_idle(),
					irq: times.cpu_times_irq(),
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
		var addresses = null;
		var count = 0;
		var result = UV.interface_addresses(Ref.make(addresses), Ref.make(count));
		if(result < 0) {
			addresses.free_interface_addresses(count);
			result.throwErr();
		}
		var result = [for(i in 0...count) {
			var addr = addresses.interface_address_get(i);
			var physAddr = addr.interface_address_phys_addr();
			{
				name:addr.interface_address_name().fromUTF8(),
				physAddr:[for(i in 0...6) physAddr.getUI8(i)],
				isInternal:addr.interface_address_is_internal() != 0,
				address:(addr.interface_address_address():SockAddr),
				netmask:(addr.interface_address_netmask():SockAddr),
			}
		}];
		addresses.free_interface_addresses(count);
		return result;
	}

	/**
		Gets the temp directory.
	**/
	static public function loadAvg():Array<Float> {
		var a = UV.loadavg_array();
		return [for(i in 0...a.length) a[i]];
	}

	/**
		Convert a string containing an IPv4 addresses to a structure.
	**/
	static public function ip4Addr(ip:String, port:Int):SockAddr {
		var addr = UV.alloc_sockaddr_storage();
		var result = UV.ip4_addr(ip.toUTF8(), port, addr.sockaddr_in_of_storage());
		if(result < 0) {
			addr.free_sockaddr_storage();
			result.throwErr();
		}
		return addr;
	}

	/**
		Convert a string containing an IPv6 addresses to a structure.
	**/
	static public function ip6Addr(ip:String, port:Int):SockAddr {
		var addr = UV.alloc_sockaddr_storage();
		var result = UV.ip6_addr(ip.toUTF8(), port, addr.sockaddr_in6_of_storage());
		if(result < 0) {
			addr.free_sockaddr_storage();
			result.throwErr();
		}
		return addr;
	}

	/**
		Convert a structure containing an IPv4 or IPv6 address to a string.
	**/
	static public function ipName(addr:SockAddr):String {
		var buf = new Bytes(256);
		var size = I64.ofInt(256);
		switch addr.sockaddr_storage_ss_family().address_family_of_af() {
			case INET: UV.ip4_name(addr.sockaddr_in_of_storage(), buf, size);
			case INET6: UV.ip6_name(addr.sockaddr_in6_of_storage(), buf, size);
			case _: throw new UVException(UV_EINVAL);
		}
		return buf.fromUTF8();
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
		UV.chdir(dir.toUTF8()).resolve();
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
		var passwd = UV.alloc_passwd();
		var result = passwd.os_get_passwd();
		if(result < 0) {
			passwd.os_free_passwd();
			result.throwErr();
		}
		var result = {
			username: passwd.passwd_username().fromUTF8(),
			uid: passwd.passwd_uid(),
			gid: passwd.passwd_gid(),
			shell: passwd.passwd_shell().fromUTF8(),
			homedir: passwd.passwd_homedir().fromUTF8(),
		}
		passwd.os_free_passwd();
		return result;
	}

	/**
		Gets the amount of free memory available in the system,
		as reported by the kernel (in bytes).
	**/
	static public inline function getFreeMemory():U64 {
		return UV.get_free_memory();
	}

	/**
		Gets the total amount of physical memory in the system (in bytes).
	**/
	static public inline function getTotalMemory():U64 {
		return UV.get_total_memory();
	}

	/**
		Gets the amount of memory available to the process (in bytes) based on limits imposed by the OS.
	**/
	static public inline function getConstrainedMemory():U64 {
		return UV.get_constrained_memory();
	}

	/**
		Returns the current high-resolution real time.

		This is expressed in nanoseconds.

		It is relative to an arbitrary time in the past. It is not related to the
		time of day and therefore not subject to clock drift. The primary use is
		for measuring performance between intervals.
	**/
	static public inline function hrTime():U64 {
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
		UV.os_getpriority(pid, Ref.make(p)).resolve();
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
		var buf = UV.alloc_utsname();
		var result = UV.os_uname(buf);
		if(result < 0) {
			buf.free_utsname();
			result.throwErr();
		}
		var result = {
			sysname: buf.utsname_sysname().fromUTF8(),
			release: buf.utsname_release().fromUTF8(),
			version: buf.utsname_version().fromUTF8(),
			machine: buf.utsname_machine().fromUTF8(),
		}
		buf.free_utsname();
		return result;
	}

	/**
		Get time.
	**/
	static public function getTimeOfDay():{sec:I64, usec:Int} {
		var tv = UV.alloc_timeval64();
		var result = UV.gettimeofday(tv);
		if(result < 0) {
			tv.free_timeval64();
			result.throwErr();
		}
		var result = {
			sec:tv.timeval64_tv_sec(),
			usec:tv.timeval64_tv_usec(),
		}
		tv.free_timeval64();
		return result;
	}

	/**
		Fill `buf` with exactly `length` cryptographically strong random bytes acquired
		from the system CSPRNG.

		`flags` is reserved for future extension and must currently be 0.
	**/
	static public function random(loop:Loop, buf:Bytes, length:Int, flags:Int, callback:(e:UVError)->Void):Void {
		loop.checkLoop();
		var req = new RandomRequest(UV.alloc_random());
		var result = loop.random_with_cb(req.r, buf.pointer_of_bytes(), I64.ofInt(length), flags, true);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = status -> {
			req.freeReq();
			callback(status.translate_uv_error());
		}
	}

	/**
		Synchronously fill `buf` with exactly `length` cryptographically strong random bytes acquired
		from the system CSPRNG.

		`flags` is reserved for future extension and must currently be 0.
	**/
	static public function randomSync(buf:Bytes, length:Int, flags:Int):Void {
		UV.random_with_cb(null, null, buf.pointer_of_bytes(), I64.ofInt(length), flags, false).resolve();
	}

}
