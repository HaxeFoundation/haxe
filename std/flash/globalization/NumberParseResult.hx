package flash.globalization;

@:require(flash10_1) extern final class NumberParseResult {
	@:flash.property var endIndex(get,never) : Int;
	@:flash.property var startIndex(get,never) : Int;
	@:flash.property var value(get,never) : Float;
	function new(value : Float = 0./*NaN*/, startIndex : Int = 2147483647, endIndex : Int = 2147483647) : Void;
	private function get_endIndex() : Int;
	private function get_startIndex() : Int;
	private function get_value() : Float;
}
