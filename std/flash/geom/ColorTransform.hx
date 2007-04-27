package flash.geom;

#if !flash8
"This class is only accesible in Flash8"
#end

extern class ColorTransform {

	var rgb : Float;
	var blueOffset : Float;
	var greenOffset : Float;
	var redOffset : Float;
	var alphaOffset : Float;
	var blueMultiplier : Float;
	var greenMultiplier : Float;
	var redMultiplier : Float;
	var alphaMultiplier : Float;

	function new( ?rm : Float, ?gm : Float, ?bm : Float, ?am : Float, ?ro : Float, ?go : Float, ?bo : Float, ?ao : Float ) : Void;
	function toString() : String;
	function concat( c : ColorTransform ) : Void;

}