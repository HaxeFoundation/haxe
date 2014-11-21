package python.lib.datetime;

@:pythonImport("datetime", "timedelta")
extern class TimeDelta {

	static var min : TimeDelta;
	static var max : TimeDelta;
	static var resolution : TimeDelta;

	var days : Int;
	var seconds : Int;
	var microseconds : Int;

	function new(days:Int = 0, seconds:Int = 0, microseconds:Int = 0, milliseconds:Int = 0, minutes:Int = 0, hours:Int = 0, weeks:Int = 0):Void;
	function total_seconds():Float;
}
