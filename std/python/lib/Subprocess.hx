
package python.lib;

extern class StartupInfo {
	public var dwFlags : Int;

	public var wShowWindow:Int;

}

@:pythonImport("subprocess")
extern class Subprocess {

	public static function STARTUPINFO():StartupInfo;

	public static var STD_INPUT_HANDLE:Int;
	public static var STD_OUTPUT_HANDLE:Int;
	public static var STD_ERROR_HANDLE:Int;
	public static var SW_HIDE:Int;
	public static var STARTF_USESTDHANDLES:Int;
	public static var STARTF_USESHOWWINDOW:Int;

	public static var CREATE_NEW_CONSOLE:Int;
	public static var CREATE_NEW_PROCESS_GROUP:Int;

	public static var PIPE:Int;

	public static var STDOUT:Int;

	public static function call(args:Array<String>):Int;

}