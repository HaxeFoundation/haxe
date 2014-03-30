
package python.lib;

extern class Tty {
	public static function setraw (fileNo:Int):Void;


	static function __init__ ():Void
	{
		try {
			python.Macros.importAs("tty", "python.lib.Tty");
		} catch (e:Dynamic) {}

	}
}