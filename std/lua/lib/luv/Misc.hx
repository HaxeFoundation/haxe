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

package lua.lib.luv;

@:luaRequire("luv")
extern class Misc {
	static function chdir(path:String):Bool;

	static function cpu_info():Table<Int, CpuInfo>;

	static function cwd():String;
	static function exepath():String;
	static function get_process_title():String;
	static function get_total_memory():Int;
	static function get_free_memory():Int;
	static function getpid():Int;

	static function getrusage():ResourceUsage;
	static function guess_handle(handle:Int):String;
	static function hrtime():Float;

    static function gettimeofday() : TimeOfDay;

	// TODO: implement this
	static function interface_addresses() : Dynamic;

	static function loadavg():Float;
	static function resident_set_memory():Int;
	static function set_process_title(title:String):Bool;
	static function uptime():Int;
	static function version():Int;
	static function version_string():String;

	// Windows only
	static function getuid():Int;
	static function setuid(from:Int, to:Int):String;
	static function getgid():Int;
	static function setgid(from:Int, to:Int):Void;

	// Windows only
	static function print_all_handles():Table<Int, String>;
	static function print_active_handles():Table<Int, String>;

}

typedef CpuInfo = {
	model:String,
	times:CpuTimes,
	speed:Int
}

typedef CpuTimes = {
	sys:Int,
	idle:Int,
	irq:Int,
	user:Int
}

typedef ResourceUsage = {
	nivcsw:Int,
	maxrss:Int,
	msgrcv:Int,
	isrss:Int,
	inblock:Int,
	ixrss:Int,
	nvcsw:Int,
	nsignals:Int,
	minflt:Int,
	nswap:Int,
	msgsnd:Int,
	oublock:Int,
	majflt:Int,
	stime:MicroTimeStamp,
	idrss:Int,
	utime:MicroTimeStamp
}

typedef MicroTimeStamp = {
	usec:Int,
	sec:Int
}

@:multiReturn
extern class TimeOfDay {
    var seconds : Int;
    var microseconds : Int;
}
