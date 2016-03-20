package flash.globalization;

@:final @:require(flash10_1) extern class DateTimeFormatter {
	var actualLocaleIDName(default,never) : String;
	var lastOperationStatus(default,never) : LastOperationStatus;
	var requestedLocaleIDName(default,never) : String;
	function new(requestedLocaleIDName : String, ?dateStyle : DateTimeStyle, ?timeStyle : DateTimeStyle) : Void;
	function format(dateTime : Date) : String;
	function formatUTC(dateTime : Date) : String;
	function getDateStyle() : DateTimeStyle;
	function getDateTimePattern() : DateTimeStyle;
	function getFirstWeekday() : Int;
	function getMonthNames(?nameStyle : DateTimeNameStyle, ?context : DateTimeNameContext) : flash.Vector<String>;
	function getTimeStyle() : DateTimeStyle;
	function getWeekdayNames(?nameStyle : DateTimeNameStyle, ?context : DateTimeNameContext) : flash.Vector<String>;
	function setDateTimePattern(pattern : String) : Void;
	function setDateTimeStyles(dateStyle : DateTimeStyle, timeStyle : DateTimeStyle) : Void;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
