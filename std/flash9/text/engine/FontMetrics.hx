package flash.text.engine;

extern class FontMetrics {
	function new(emBox : flash.geom.Rectangle, strikethroughOffset : Float, strikethroughThickness : Float, underlineOffset : Float, underlineThickness : Float, subscriptOffset : Float, subscriptScale : Float, superscriptOffset : Float, superscriptScale : Float) : Void;
	var emBox : flash.geom.Rectangle;
	var strikethroughOffset : Float;
	var strikethroughThickness : Float;
	var subscriptOffset : Float;
	var subscriptScale : Float;
	var superscriptOffset : Float;
	var superscriptScale : Float;
	var underlineOffset : Float;
	var underlineThickness : Float;
}
