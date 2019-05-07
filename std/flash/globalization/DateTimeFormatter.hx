package flash.globalization;

@:require(flash10_1) extern final class DateTimeFormatter {
	@:flash.property var actualLocaleIDName(get,never) : String;
	@:flash.property var lastOperationStatus(get,never) : LastOperationStatus;
	@:flash.property var requestedLocaleIDName(get,never) : String;
	function new(requestedLocaleIDName : String, ?dateStyle : DateTimeStyle, ?timeStyle : DateTimeStyle) : Void;
	function format(dateTime : Date) : String;
	function formatUTC(dateTime : Date) : String;
	function getDateStyle() : DateTimeStyle;
	function getDateTimePattern() : DateTimeStyle;
	function getFirstWeekday() : Int;
	function getMonthNames(?nameStyle : DateTimeNameStyle, ?context : DateTimeNameContext) : flash.Vector<String>;
	function getTimeStyle() : DateTimeStyle;
	function getWeekdayNames(?nameStyle : DateTimeNameStyle, ?context : DateTimeNameContext) : flash.Vector<String>;
	private function get_actualLocaleIDName() : String;
	private function get_lastOperationStatus() : LastOperationStatus;
	private function get_requestedLocaleIDName() : String;
	function setDateTimePattern(pattern : String) : Void;
	function setDateTimeStyles(dateStyle : DateTimeStyle, timeStyle : DateTimeStyle) : Void;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
