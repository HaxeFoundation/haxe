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

typedef RUsage = {
	/** user CPU time used */
	var utime:{sec:I64, usec:I64};
	/** system CPU time used */
	var stime:{sec:I64, usec:I64};
	/** maximum resident set size */
	var maxrss:I64;
	/** integral shared memory size (X) */
	var ixrss:I64;
	/** integral unshared data size (X) */
	var idrss:I64;
	/** integral unshared stack size (X) */
	var isrss:I64;
	/** page reclaims (soft page faults) (X) */
	var minflt:I64;
	/** page faults (hard page faults) */
	var majflt:I64;
	/** swaps (X) */
	var nswap:I64;
	/** block input operations */
	var inblock:I64;
	/** block output operations */
	var oublock:I64;
	/** IPC messages sent (X) */
	var msgsnd:I64;
	/** IPC messages received (X) */
	var msgrcv:I64;
	/** signals received (X) */
	var nsignals:I64;
	/** voluntary context switches (X) */
	var nvcsw:I64;
	/** involuntary context switches (X) */
	var nivcsw:I64;
}

typedef CpuInfo = {
	var model:String;
	var speed:Int;
	var cpuTimes:{
		/** milliseconds */
		var user:I64;
		/** milliseconds */
		var nice:I64;
		/** milliseconds */
		var sys:I64;
		/** milliseconds */
		var idle:I64
		/** milliseconds */;
		var irq:I64;
	}
}

typedef InterfaceAddress = {
	var name:String;
	var physAddr:haxe.io.Bytes;
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

/**
	Miscellaneous.

	@see http://docs.libuv.org/en/v1.x/misc.html
**/
class Misc {
	/**
		Gets the resident set size (RSS) for the current process.
	**/
	@:hlNative("uv", "resident_set_memory_wrap")
	static public function residentSetMemory():I64
		return I64.ofInt(0);

	/**
		Gets the current system uptime.
	**/
	@:hlNative("uv", "uptime_wrap")
	static public function uptime():Float
		return 0;

	/**
		Gets the resource usage measures for the current process.
	**/
	static public inline function getRUsage():RUsage
		return getRUsageWrap();

	@:hlNative("uv", "getrusage_wrap")
	static function getRUsageWrap():Dynamic
		return null;

	/**
		Returns the current process ID.
	**/
	@:hlNative("uv", "os_getpid")
	static public function getPid():Int
		return 0;

	/**
		Returns the parent process ID.
	**/
	@:hlNative("uv", "os_getppid")
	static public function getPPid():Int
		return 0;

	/**
		Gets information about the CPUs on the system.
	**/
	static public function cpuInfo():Array<CpuInfo> {
		var infos = cpuInfoWrap();
		var a = new Array<CpuInfo>();
		a.resize(infos.length);
		for(i in 0...infos.length) {
			var info:Dynamic = infos[i];
			info.model = @:privateAccess String.fromUTF8(info.model);
			a[i] = info;
		}
		return a;
	}

	@:hlNative("uv", "cpu_info_wrap")
	static function cpuInfoWrap():NativeArray<Dynamic>
		return null;

	/**
		Gets address information about the network interfaces on the system.
	**/
	static public function interfaceAddresses():Array<InterfaceAddress> {
		var addresses = interfaceAddressesWrap();
		var a = new Array<InterfaceAddress>();
		a.resize(addresses.length);
		for(i in 0...addresses.length) {
			var addr:Dynamic = addresses[i];
			addr.name = @:privateAccess String.fromUTF8(addr.name);
			addr.physAddr = haxe.io.Bytes.ofData(new haxe.io.BytesData(addr.physAddr, 6));
			addr.address = @:privateAccess SockAddr.castPtr(addr.address);
			addr.netmask = @:privateAccess SockAddr.castPtr(addr.netmask);
			a[i] = addr;
		}
		return a;
	}

	@:hlNative("uv", "interface_addresses_wrap")
	static function interfaceAddressesWrap():NativeArray<Dynamic>
		return null;

	/**
		Gets the load average.
	**/
	static public function loadAvg():Array<Float> {
		var result = new Array<Float>();
		result.resize(3);
		var avg = loadAvgWrap();
		for(i in 0...avg.length) {
			result[i] = avg[i];
		}
		return result;
	}

	@:hlNative("uv", "loadavg_wrap")
	static public function loadAvgWrap():NativeArray<Float>
		return null;

	/**
		Gets the temp directory.
	**/
	static public inline function tmpDir():String
		return @:privateAccess String.fromUTF8(tmpDirWrap());

	@:hlNative("uv", "os_tmpdir_wrap")
	static function tmpDirWrap():Bytes
		return null;

	/**
		Gets the current user’s home directory.
	**/
	static public inline function homeDir():String
		return @:privateAccess String.fromUTF8(homeDirWrap());

	@:hlNative("uv", "os_homedir_wrap")
	static function homeDirWrap():Bytes
		return null;

	/**
		Gets a subset of the password file entry for the current effective uid (not the real uid).
	**/
	static public function getPasswd():Passwd @:privateAccess {
		var p:Dynamic = getPasswdWrap();
		p.username = String.fromUTF8(p.username);
		p.shell = p.shell == null ? null : String.fromUTF8(p.shell);
		p.homedir = String.fromUTF8(p.homedir);
		return p;
	}

	@:hlNative("uv", "os_getpasswd_wrap")
	static function getPasswdWrap():Dynamic
		return null;
}
