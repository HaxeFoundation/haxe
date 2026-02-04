package cs.system;

import cs.system.io.Stream;

@:native("System.Console")
extern class Console {
	static function Write(value:Dynamic):Void;
	static function WriteLine(value:Dynamic):Void;
	static function ReadKey(intercept:Bool):ConsoleKeyInfo;
	static function OpenStandardInput():Stream;
	static function OpenStandardOutput():Stream;
	static function OpenStandardError():Stream;
}
