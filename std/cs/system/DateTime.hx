package cs.system;

@:native("System.DateTime")
extern class DateTime {
	static var UtcNow(default, never):DateTime;
	static var Now(default, never):DateTime;
	static var MinValue(default, never):DateTime;
	static var MaxValue(default, never):DateTime;

	var Ticks(default, never):haxe.Int64;
	var Year(default, never):Int;
	var Month(default, never):Int;
	var Day(default, never):Int;
	var Hour(default, never):Int;
	var Minute(default, never):Int;
	var Second(default, never):Int;
	var Millisecond(default, never):Int;
	var DayOfWeek(default, never):Int;
	var DayOfYear(default, never):Int;

	@:overload(function(year:Int, month:Int, day:Int):Void {})
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int):Void {})
	function new(ticks:haxe.Int64):Void;

	function AddDays(value:Float):DateTime;
	function AddHours(value:Float):DateTime;
	function AddMilliseconds(value:Float):DateTime;
	function AddMinutes(value:Float):DateTime;
	function AddMonths(months:Int):DateTime;
	function AddSeconds(value:Float):DateTime;
	function AddTicks(value:haxe.Int64):DateTime;
	function AddYears(value:Int):DateTime;
	function ToUniversalTime():DateTime;
	function ToLocalTime():DateTime;
	function ToString():String;
}
