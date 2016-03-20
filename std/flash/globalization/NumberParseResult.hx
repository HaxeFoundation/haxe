package flash.globalization;

@:final @:require(flash10_1) extern class NumberParseResult {
	var endIndex(default,never) : Int;
	var startIndex(default,never) : Int;
	var value(default,never) : Float;
	function new(value : Float = 0./*NaN*/, startIndex : Int = 2147483647, endIndex : Int = 2147483647) : Void;
}
