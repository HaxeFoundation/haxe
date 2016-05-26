package flash.globalization;

@:final extern class CurrencyParseResult {
	var currencyString(default,never) : String;
	var value(default,never) : Float;
	function new(value : Float = 0./*NaN*/, ?symbol : String) : Void;
}
