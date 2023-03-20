package flash.globalization;

@:require(flash10_1) extern final class DateTimeFormatter {
	@:flash.property var actualLocaleIDName(get,never) : String;
	@:flash.property var lastOperationStatus(get,never) : String;
	@:flash.property var requestedLocaleIDName(get,never) : String;
	function new(requestedLocaleIDName : String, ?dateStyle : String, ?timeStyle : String) : Void;
	function format(dateTime : Date) : String;
	function formatUTC(dateTime : Date) : String;
	function getDateStyle() : String;
	function getDateTimePattern() : String;
	function getFirstWeekday() : Int;
	function getMonthNames(?nameStyle : String, ?context : String) : flash.Vector<String>;
	function getTimeStyle() : String;
	function getWeekdayNames(?nameStyle : String, ?context : String) : flash.Vector<String>;
	private function get_actualLocaleIDName() : String;
	private function get_lastOperationStatus() : String;
	private function get_requestedLocaleIDName() : String;
	function setDateTimePattern(pattern : String) : Void;
	function setDateTimeStyles(dateStyle : String, timeStyle : String) : Void;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
