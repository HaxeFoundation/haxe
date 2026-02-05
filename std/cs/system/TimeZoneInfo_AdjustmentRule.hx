package cs.system;

@:native("System.TimeZoneInfo.AdjustmentRule")
extern class TimeZoneInfo_AdjustmentRule {
	var DateEnd(default, never):cs.system.DateTime;
	var DateStart(default, never):cs.system.DateTime;
	var DaylightDelta(default, never):cs.system.TimeSpan;
	var DaylightTransitionEnd(default, never):cs.system.TimeZoneInfo_TransitionTime;
	var DaylightTransitionStart(default, never):cs.system.TimeZoneInfo_TransitionTime;
	static function CreateAdjustmentRule(dateStart:cs.system.DateTime, dateEnd:cs.system.DateTime, daylightDelta:cs.system.TimeSpan, daylightTransitionStart:cs.system.TimeZoneInfo_TransitionTime, daylightTransitionEnd:cs.system.TimeZoneInfo_TransitionTime):cs.system.TimeZoneInfo_AdjustmentRule;
	function Equals(other:cs.system.TimeZoneInfo_AdjustmentRule):Bool;
	function GetHashCode():Int;
}
