package cs.system;

@:native("System.TimeZoneInfo.TransitionTime")
extern class TimeZoneInfo_TransitionTime extends cs.system.ValueType {
	var Day(default, never):Int;
	var DayOfWeek(default, never):cs.system.DayOfWeek;
	var IsFixedDateRule(default, never):Bool;
	var Month(default, never):Int;
	var TimeOfDay(default, never):cs.system.DateTime;
	var Week(default, never):Int;
	static function CreateFixedDateRule(timeOfDay:cs.system.DateTime, month:Int, day:Int):cs.system.TimeZoneInfo_TransitionTime;
	static function CreateFloatingDateRule(timeOfDay:cs.system.DateTime, month:Int, week:Int, dayOfWeek:cs.system.DayOfWeek):cs.system.TimeZoneInfo_TransitionTime;
	static function op_Equality(t1:cs.system.TimeZoneInfo_TransitionTime, t2:cs.system.TimeZoneInfo_TransitionTime):Bool;
	static function op_Inequality(t1:cs.system.TimeZoneInfo_TransitionTime, t2:cs.system.TimeZoneInfo_TransitionTime):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	function Equals(other:cs.system.TimeZoneInfo_TransitionTime):Bool;
	function GetHashCode():Int;
}
