
package python.lib;

extern class Time {

	public static function time ():Float;
	public static function clock ():Float;
	public static function sleep (t:Float):Void;
	static function __init__ ():Void
	{
		python.Syntax.importAs("time", "python.lib.Time");
	}
}