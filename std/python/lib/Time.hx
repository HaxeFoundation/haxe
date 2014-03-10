
package python.lib;

extern class Time {

	public static function time ():Int;
	public static function sleep (t:Float):Void;
	static function __init__ ():Void 
	{
		python.Macros.importAs("time", "python.lib.Time");
	}
}