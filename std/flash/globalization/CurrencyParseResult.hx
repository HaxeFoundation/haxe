package flash.globalization;

extern final class CurrencyParseResult {
	var currencyString(get,never) : String;
	var value(get,never) : Float;
	function new(value : Float = 0./*NaN*/, ?symbol : String) : Void;
	private function get_currencyString() : String;
	private function get_value() : Float;
}
