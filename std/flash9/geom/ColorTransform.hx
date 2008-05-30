package flash.geom;

extern class ColorTransform {
	var alphaMultiplier : Float;
	var alphaOffset : Float;
	var blueMultiplier : Float;
	var blueOffset : Float;
	var color : UInt;
	var greenMultiplier : Float;
	var greenOffset : Float;
	var redMultiplier : Float;
	var redOffset : Float;
	function new(?redMultiplier : Float, ?greenMultiplier : Float, ?blueMultiplier : Float, ?alphaMultiplier : Float, ?redOffset : Float, ?greenOffset : Float, ?blueOffset : Float, ?alphaOffset : Float) : Void;
	function concat(second : ColorTransform) : Void;
	function toString() : String;
}
