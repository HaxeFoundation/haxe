package eval.luv;

import eval.integers.UInt64;
import eval.integers.Int64;

typedef RUsage = {
	var utime:{sec:Int64, usec:Int64};
	var stime:{sec:Int64, usec:Int64};
	var maxrss:UInt64;
	var ixrss:UInt64;
	var idrss:UInt64;
	var isrss:UInt64;
	var minflt:UInt64;
	var majflt:UInt64;
	var nswap:UInt64;
	var inblock:UInt64;
	var oublock:UInt64;
	var msgsnd:UInt64;
	var msgrcv:UInt64;
	var nsignals:UInt64;
	var nvcsw:UInt64;
	var nivcsw:UInt64;
}

/**
	Resource usage.

	@see https://aantron.github.io/luv/luv/Luv/Resource
**/
extern class Resource {
	/**
		Evaluates to the current uptime.
	**/
	static function uptime():Result<Float>;

	/**
		Evaluates to the load average.
	**/
	static function loadAvg():Array<Float>;

	/**
		Evaluates to the amount of free memory, in bytes.
		Returns `null` when unknown.
	**/
	static function freeMemory():Null<UInt64>;

	/**
		Evaluates to the total amount of memory, in bytes.
		Returns `null` when unknown.
	**/
	static function totalMemory():Null<UInt64>;

	/**
		Gets the amount of memory available to the process (in bytes) based on
		limits imposed by the OS.
		If there is no such constraint returns `null`
	**/
	static function constrainedMemory():Null<UInt64>;

	/**
		Evaluates to the priority of the process with the given pid.
	**/
	static function getPriority(pid:Int):Result<Int>;

	/**
		Sets the priority of the process with the given pid.
	**/
	static function setPriority(pid:Int, priority:Int):Result<Result.NoData>;

	/**
		Evaluates to the resident set size for the current process.
	**/
	static function residentSetMemory():Result<UInt64>;

	/**
		Gets the resource usage measures for the current process.
	**/
	static function getRUsage():Result<RUsage>;

}