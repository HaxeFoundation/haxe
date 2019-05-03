package flash.globalization;

@:require(flash10_1) extern final class Collator {
	var actualLocaleIDName(get,never) : String;
	var ignoreCase(get,set) : Bool;
	var ignoreCharacterWidth(get,set) : Bool;
	var ignoreDiacritics(get,set) : Bool;
	var ignoreKanaType(get,set) : Bool;
	var ignoreSymbols(get,set) : Bool;
	var lastOperationStatus(get,never) : LastOperationStatus;
	var numericComparison(get,set) : Bool;
	var requestedLocaleIDName(get,never) : String;
	function new(requestedLocaleIDName : String, ?initialMode : CollatorMode) : Void;
	function compare(string1 : String, string2 : String) : Int;
	function equals(string1 : String, string2 : String) : Bool;
	private function get_actualLocaleIDName() : String;
	private function get_ignoreCase() : Bool;
	private function get_ignoreCharacterWidth() : Bool;
	private function get_ignoreDiacritics() : Bool;
	private function get_ignoreKanaType() : Bool;
	private function get_ignoreSymbols() : Bool;
	private function get_lastOperationStatus() : LastOperationStatus;
	private function get_numericComparison() : Bool;
	private function get_requestedLocaleIDName() : String;
	private function set_ignoreCase(value : Bool) : Bool;
	private function set_ignoreCharacterWidth(value : Bool) : Bool;
	private function set_ignoreDiacritics(value : Bool) : Bool;
	private function set_ignoreKanaType(value : Bool) : Bool;
	private function set_ignoreSymbols(value : Bool) : Bool;
	private function set_numericComparison(value : Bool) : Bool;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
