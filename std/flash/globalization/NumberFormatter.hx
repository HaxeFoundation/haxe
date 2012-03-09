package flash.globalization;

@:final @:require(flash10_1) extern class NumberFormatter {
	var actualLocaleIDName(default,null) : String;
	var decimalSeparator : String;
	var digitsType : NationalDigitsType;
	var fractionalDigits : Int;
	var groupingPattern : String;
	var groupingSeparator : String;
	var lastOperationStatus(default,null) : LastOperationStatus;
	var leadingZero : Bool;
	var negativeNumberFormat : UInt;
	var negativeSymbol : String;
	var requestedLocaleIDName(default,null) : String;
	var trailingZeros : Bool;
	var useGrouping : Bool;
	function new(requestedLocaleIDName : String) : Void;
	function formatInt(value : Int) : String;
	function formatNumber(value : Float) : String;
	function formatUint(value : UInt) : String;
	function parse(parseString : String) : NumberParseResult;
	function parseNumber(parseString : String) : Float;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
