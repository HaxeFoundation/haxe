package flash.globalization;

@:final extern class CurrencyParseResult {
	var currencyString(default,null) : String;
	var value(default,null) : Float;
	function new(value : Float = 0./*NaN*/, ?symbol : String) : Void;
}
