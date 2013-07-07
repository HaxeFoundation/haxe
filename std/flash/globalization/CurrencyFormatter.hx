package flash.globalization;

@:final @:require(flash10_1) extern class CurrencyFormatter {
	var actualLocaleIDName(default,null) : String;
	var currencyISOCode(default,null) : String;
	var currencySymbol(default,null) : String;
	var decimalSeparator : String;
	var digitsType : UInt;
	var fractionalDigits : Int;
	var groupingPattern : String;
	var groupingSeparator : String;
	var lastOperationStatus(default,null) : LastOperationStatus;
	var leadingZero : Bool;
	var negativeCurrencyFormat : UInt;
	var negativeSymbol : String;
	var positiveCurrencyFormat : UInt;
	var requestedLocaleIDName(default,null) : String;
	var trailingZeros : Bool;
	var useGrouping : Bool;
	function new(requestedLocaleIDName : String) : Void;
	function format(value : Float, withCurrencySymbol : Bool = false) : String;
	function formattingWithCurrencySymbolIsSafe(requestedISOCode : String) : Bool;
	function parse(inputString : String) : CurrencyParseResult;
	function setCurrency(currencyISOCode : String, currencySymbol : String) : Void;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
