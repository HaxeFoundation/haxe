package flash.geom;

extern class ColorTransform {
	var alphaMultiplier : Float;
	var alphaOffset : Float;
	var blueMultiplier : Float;
	var blueOffset : Float;
	@:flash.property var color(get,set) : UInt;
	var greenMultiplier : Float;
	var greenOffset : Float;
	var redMultiplier : Float;
	var redOffset : Float;
	function new(redMultiplier : Float = 1, greenMultiplier : Float = 1, blueMultiplier : Float = 1, alphaMultiplier : Float = 1, redOffset : Float = 0, greenOffset : Float = 0, blueOffset : Float = 0, alphaOffset : Float = 0) : Void;
	function concat(second : ColorTransform) : Void;
	private function get_color() : UInt;
	private function set_color(value : UInt) : UInt;
	function toString() : String;
}
