
package python.lib;



extern class Random {

	public static function random ():Float;

	static function __init__ ():Void {
		Syntax.importAs("random", "python.lib.Random");
	}

}