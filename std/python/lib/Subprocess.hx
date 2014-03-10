
package python.lib;

extern class StartupInfo {
	public var dwFlags : Int;

	public var wShowWindow:Int;

}

extern class Subprocess {

	public static function STARTUPINFO():StartupInfo;

	public static var STD_INPUT_HANDLE:Int;
	public static var STD_OUTPUT_HANDLE:Int;
	public static var STD_ERROR_HANDLE:Int;
	public static var SW_HIDE:Int;
	public static var STARTF_USESTDHANDLES:Int;
	public static var STARTF_USESHOWWINDOW:Int;

	//public static var CREATE_NEW_CONSOLE;

	//public static var CREATE_NEW_PROCESS_GROUP;

	public static var PIPE:Dynamic;

	public static var STDOUT:Dynamic;	

	static function __init__ ():Void 
	{
		python.Macros.importAs("subprocess", "python.lib.Subprocess");
	}
}