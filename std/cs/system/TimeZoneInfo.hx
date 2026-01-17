package cs.system;

@:native("System.TimeZoneInfo")
extern class TimeZoneInfo {
	static var Local(default, never):TimeZoneInfo;
	static var Utc(default, never):TimeZoneInfo;

	var Id(default, never):String;
	var DisplayName(default, never):String;
	var StandardName(default, never):String;
	var BaseUtcOffset(default, never):TimeSpan;

	function GetUtcOffset(dateTime:DateTime):TimeSpan;
	function IsDaylightSavingTime(dateTime:DateTime):Bool;

	static function FindSystemTimeZoneById(id:String):TimeZoneInfo;
}
