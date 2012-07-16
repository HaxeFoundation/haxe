package cs.system;

/**
	Warning: This class definition is incomplete.
	In order to get most current extern definitions, install/update hxcs library with:
		haxelib install hxcs
	Please refer to http://lib.haxe.org/p/hxcs for more information.
**/
@:native('System.Environment') extern class Environment
{
	static function Exit(code:Int):Void;
	static function GetEnvironmentVariables():cs.system.collections.IDictionary;
	static function GetEnvironmentVariable(k:String):String;
	static function SetEnvironmentVariable(k:String, v:String):Void;
	static function GetCommandLineArgs():cs.NativeArray<String>;
	static var NewLine(default, null):String;
	static var TickCount(default, null):Int;
	static var OSVersion(default, null):OperatingSystem;
	
}