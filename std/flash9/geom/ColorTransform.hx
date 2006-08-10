package flash.geom;

extern class ColorTransform {
	function new(?redMultiplier : Float, ?greenMultiplier : Float, ?blueMultiplier : Float, ?alphaMultiplier : Float, ?redOffset : Float, ?greenOffset : Float, ?blueOffset : Float, ?alphaOffset : Float) : Void;
	var alphaMultiplier : Float;
	var alphaOffset : Float;
	var blueMultiplier : Float;
	var blueOffset : Float;
	var color : UInt;
	function concat(second : flash.geom.ColorTransform) : Void;
	var greenMultiplier : Float;
	var greenOffset : Float;
	var redMultiplier : Float;
	var redOffset : Float;
	function toString() : String;
}
