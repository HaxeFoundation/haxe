package cs.system;

@:native("System.TimeSpan")
extern class TimeSpan {
	static var TicksPerSecond(default, never):haxe.Int64;
	static var TicksPerMillisecond(default, never):haxe.Int64;
	static var TicksPerMinute(default, never):haxe.Int64;
	static var TicksPerHour(default, never):haxe.Int64;
	static var TicksPerDay(default, never):haxe.Int64;
	static var Zero(default, never):TimeSpan;
	static var MinValue(default, never):TimeSpan;
	static var MaxValue(default, never):TimeSpan;

	var Ticks(default, never):haxe.Int64;
	var Days(default, never):Int;
	var Hours(default, never):Int;
	var Minutes(default, never):Int;
	var Seconds(default, never):Int;
	var Milliseconds(default, never):Int;
	var TotalDays(default, never):Float;
	var TotalHours(default, never):Float;
	var TotalMinutes(default, never):Float;
	var TotalSeconds(default, never):Float;
	var TotalMilliseconds(default, never):Float;

	@:overload(function(hours:Int, minutes:Int, seconds:Int):Void {})
	@:overload(function(days:Int, hours:Int, minutes:Int, seconds:Int):Void {})
	@:overload(function(days:Int, hours:Int, minutes:Int, seconds:Int, milliseconds:Int):Void {})
	function new(ticks:haxe.Int64):Void;

	function Add(ts:TimeSpan):TimeSpan;
	function Subtract(ts:TimeSpan):TimeSpan;
	function Negate():TimeSpan;
	function Duration():TimeSpan;
	function ToString():String;

	static function FromDays(value:Float):TimeSpan;
	static function FromHours(value:Float):TimeSpan;
	static function FromMinutes(value:Float):TimeSpan;
	static function FromSeconds(value:Float):TimeSpan;
	static function FromMilliseconds(value:Float):TimeSpan;
	static function FromTicks(value:haxe.Int64):TimeSpan;
}
