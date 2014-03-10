
package python.lib;

extern class Tempfile {

	public static function gettempdir():String;
	static function __init__ ():Void {
		Macros.importAs("tempfile", "python.lib.Tempfile");
	}

}