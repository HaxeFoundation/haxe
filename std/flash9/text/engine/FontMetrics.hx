package flash.text.engine;

extern class FontMetrics {
	function new(emBox : flash.geom.Rectangle, strikethroughPosition : Float, strikethroughThickness : Float, underlinePosition : Float, underlineThickness : Float, subscriptScale : Float, superscriptScale : Float) : Void;
	var emBox : flash.geom.Rectangle;
	var strikethroughPosition : Float;
	var strikethroughThickness : Float;
	var subscriptScale : Float;
	var superscriptScale : Float;
	var underlinePosition : Float;
	var underlineThickness : Float;
}
