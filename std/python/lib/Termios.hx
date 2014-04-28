
package python.lib;

abstract TermiosSettings(Dynamic) {}

extern class Termios {

	public static var TCSADRAIN : Int;
	public static var ECHO : Int;

	public static function tcgetattr (fileNo:Int):TermiosSettings;

	public static function tcsetattr (fileNo:Int, when:Int, settings:TermiosSettings):Void;

	static function __init__ ():Void
	{
		try {
			python.Syntax.importAs("termios", "python.lib.Termios");
		}
		catch (e:Dynamic) {}
	}

}