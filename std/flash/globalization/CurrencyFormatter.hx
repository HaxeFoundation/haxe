package flash.globalization;

@:final @:require(flash10_1) extern class CurrencyFormatter {
	var actualLocaleIDName(default,never) : String;
	var currencyISOCode(default,never) : String;
	var currencySymbol(default,never) : String;
	var decimalSeparator : String;
	var digitsType : UInt;
	var fractionalDigits : Int;
	var groupingPattern : String;
	var groupingSeparator : String;
	var lastOperationStatus(default,never) : LastOperationStatus;
	var leadingZero : Bool;
	var negativeCurrencyFormat : UInt;
	var negativeSymbol : String;
	var positiveCurrencyFormat : UInt;
	var requestedLocaleIDName(default,never) : String;
	var trailingZeros : Bool;
	var useGrouping : Bool;
	function new(requestedLocaleIDName : String) : Void;
	function format(value : Float, withCurrencySymbol : Bool = false) : String;
	function formattingWithCurrencySymbolIsSafe(requestedISOCode : String) : Bool;
	function parse(inputString : String) : CurrencyParseResult;
	function setCurrency(currencyISOCode : String, currencySymbol : String) : Void;
	static function getAvailableLocaleIDNames() : flash.Vector<String>;
}
