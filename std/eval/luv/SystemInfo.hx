package eval.luv;

import eval.integers.UInt64;

typedef CpuInfo = {
	var model:String;
	var speed:Int;
	var times:{
		var user:UInt64;
		var nice:UInt64;
		var sys:UInt64;
		var idle:UInt64;
		var irq:UInt64;
	};
}

typedef Uname = {
	var sysname:String;
	var release:String;
	var version:String;
	var machine:String;
}

/**
	System information.

	@see https://aantron.github.io/luv/luv/Luv/System_info
**/
extern class SystemInfo {
	/**
		Gets information about the CPUs on the system.
	**/
	static function cpuInfo():Result<Array<CpuInfo>>;

	/**
		Gets information about the CPUs on the system.
	**/
	static function uname():Result<Uname>;
}