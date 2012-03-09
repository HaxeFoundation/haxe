package flash.globalization;

@:final @:require(flash10_1) extern class StringTools {
	var actualLocaleIDName(default,null) : String;
	var lastOperationStatus(default,null) : LastOperationStatus;
	var requestedLocaleIDName(default,null) : String;
	function new(requestedLocaleIDName : String) : Void;
	function toLowerCase(s : String) : String;
	function toUpperCase(s : String) : String;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
