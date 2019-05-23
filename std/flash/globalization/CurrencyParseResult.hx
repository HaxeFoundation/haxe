package flash.globalization;

extern final class CurrencyParseResult {
	@:flash.property var currencyString(get,never) : String;
	@:flash.property var value(get,never) : Float;
	function new(value : Float = 0./*NaN*/, ?symbol : String) : Void;
	private function get_currencyString() : String;
	private function get_value() : Float;
}
