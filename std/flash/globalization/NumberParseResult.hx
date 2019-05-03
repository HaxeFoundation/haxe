package flash.globalization;

@:require(flash10_1) extern final class NumberParseResult {
	var endIndex(get,never) : Int;
	var startIndex(get,never) : Int;
	var value(get,never) : Float;
	function new(value : Float = 0./*NaN*/, startIndex : Int = 2147483647, endIndex : Int = 2147483647) : Void;
	private function get_endIndex() : Int;
	private function get_startIndex() : Int;
	private function get_value() : Float;
}
