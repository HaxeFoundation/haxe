package python.lib.datetime;

@:pythonImport("datetime", "timedelta")
extern class Timedelta {

	static var min : Timedelta;
	static var max : Timedelta;
	static var resolution : Timedelta;

	var days : Int;
	var seconds : Int;
	var microseconds : Int;

	function new(days:Int = 0, seconds:Int = 0, microseconds:Int = 0, milliseconds:Int = 0, minutes:Int = 0, hours:Int = 0, weeks:Int = 0):Void;
	function total_seconds():Float;
}
