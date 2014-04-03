
package python.lib;

extern class Msvcrt {

	public static function getch ():python.lib.Types.Bytes;

	static function __init__ ():Void
	{
		try {
			python.Syntax.importAs("msvcrt", "python.lib.Msvcrt");
		} catch (e:Dynamic) {}
	}

}