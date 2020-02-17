package flash.globalization;

@:require(flash10_1) extern final class StringTools {
	@:flash.property var actualLocaleIDName(get,never) : String;
	@:flash.property var lastOperationStatus(get,never) : LastOperationStatus;
	@:flash.property var requestedLocaleIDName(get,never) : String;
	function new(requestedLocaleIDName : String) : Void;
	private function get_actualLocaleIDName() : String;
	private function get_lastOperationStatus() : LastOperationStatus;
	private function get_requestedLocaleIDName() : String;
	function toLowerCase(s : String) : String;
	function toUpperCase(s : String) : String;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
