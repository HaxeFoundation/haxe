
package python.lib;

extern class Msvrt {

	public static function getch ():String;

	static function __init__ ():Void
	{
		try {
			python.Macros.importAs("msvrt", "python.lib.Msvrt");
		} catch (e:Dynamic) {}
	}

}