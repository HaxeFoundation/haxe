package flash.filters;

extern class DisplacementMapFilter extends BitmapFilter {

	var alpha : Float;
	var color : Float;
	var mode : String;
	var scaleX : Float;
	var scaleY : Float;
	var componentX : Float;
	var componentY : Float;
	var mapPoint : flash.geom.Point<Float>;
	var mapBitmap : flash.display.BitmapData;

	function new( ?mapBitmap : flash.display.BitmapData, ?mapPoint : flash.geom.Point<Float>, ?componentX : Float, ?componentY : Float, ?scaleX : Float, ?scaleY : Float, ?mode : String, ?color : Float, ?alpha : Float ) : Void;
	function clone() : DisplacementMapFilter;

}