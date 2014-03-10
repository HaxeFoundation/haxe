
package python.lib.datetime;

extern class TimeDelta {

	public static var min : TimeDelta;
	public static var max : TimeDelta;
	public static var resolution : TimeDelta;

	
	var days : Int;
	
	var seconds : Int;
	var microseconds : Int;
	

	static function __init__ ():Void 
	{
		python.Macros.importFromAs("datetime", "timedelta", "python.lib.datetime.TimeDelta");
	}
}