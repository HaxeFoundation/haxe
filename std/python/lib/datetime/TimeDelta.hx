
package python.lib.datetime;

extern class TimeDelta {

	public static var min : TimeDelta;
	public static var max : TimeDelta;
	public static var resolution : TimeDelta;


	var days : Int;

	var seconds : Int;
	var microseconds : Int;

	public function new(days:Int = 0, seconds:Int = 0, microseconds:Int = 0, milliseconds:Int = 0, minutes:Int = 0, hours:Int = 0, weeks:Int = 0):Void;

	static function __init__ ():Void
	{
		python.Syntax.importFromAs("datetime", "timedelta", "python.lib.datetime.TimeDelta");
	}
}