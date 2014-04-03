
package python.lib;

extern class Tempfile {

	public static function gettempdir():String;
	static function __init__ ():Void {
		Syntax.importAs("tempfile", "python.lib.Tempfile");
	}

}