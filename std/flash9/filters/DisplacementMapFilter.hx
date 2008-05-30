package flash.filters;

extern class DisplacementMapFilter extends BitmapFilter {
	var alpha : Float;
	var color : UInt;
	var componentX : UInt;
	var componentY : UInt;
	var mapBitmap : flash.display.BitmapData;
	var mapPoint : flash.geom.Point;
	var mode : DisplacementMapFilterMode;
	var scaleX : Float;
	var scaleY : Float;
	function new(?mapBitmap : flash.display.BitmapData, ?mapPoint : flash.geom.Point, ?componentX : UInt, ?componentY : UInt, ?scaleX : Float, ?scaleY : Float, ?mode : DisplacementMapFilterMode, ?color : UInt, ?alpha : Float) : Void;
}
