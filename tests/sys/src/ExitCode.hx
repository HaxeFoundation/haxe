/**
	This is intented to be used by TestSys and io.TestProcess.
*/
class ExitCode {
	static public var bin:String =
	#if neko
		"bin/neko/ExitCode.n";
	#elseif cpp
		#if debug
			"bin/cpp/ExitCode-debug";
		#else
			"bin/cpp/ExitCode";
		#end
	#elseif cs
		#if debug
			"bin/cs/bin/ExitCode-Debug.exe";
		#else
			"bin/cs/bin/ExitCode.exe";
		#end
	#elseif java
		#if debug
			"bin/java/ExitCode-Debug.jar";
		#else
			"bin/java/ExitCode.jar";
		#end
	#elseif python
		"bin/python/ExitCode.py";
	#elseif php
		"bin/php/ExitCode/index.php";
	#else
		null;
	#end
	
	static function main():Void {
		Sys.exit(Std.parseInt(Sys.args()[0]));
	}
}