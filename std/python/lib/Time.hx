
package python.lib;

@:pythonImport("time")
extern class Time {

	public static function time ():Float;
	public static function clock ():Float;
	public static function sleep (t:Float):Void;
}