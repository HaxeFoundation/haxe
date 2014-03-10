
package python.lib;



extern class Random {

	public static function random ():Float;

	static function __init__ ():Void {
		Macros.importAs("random", "python.lib.Random");
	}

}