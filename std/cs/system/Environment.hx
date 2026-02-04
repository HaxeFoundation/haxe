package cs.system;

@:native("System.Environment")
extern class Environment {
	static function GetEnvironmentVariable(variable:String):String;
	static function SetEnvironmentVariable(variable:String, value:String):Void;
	static function GetEnvironmentVariables():cs.system.collections.IDictionary;
	static function GetCommandLineArgs():cs.NativeArray<String>;
	static function Exit(exitCode:Int):Void;
	static var OSVersion(default, never):OperatingSystem;
	static var TickCount(default, never):Int;
}
