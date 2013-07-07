package flash.filters;

@:final extern class DisplacementMapFilter extends BitmapFilter {
	var alpha : Float;
	var color : UInt;
	var componentX : UInt;
	var componentY : UInt;
	var mapBitmap : flash.display.BitmapData;
	var mapPoint : flash.geom.Point;
	var mode : DisplacementMapFilterMode;
	var scaleX : Float;
	var scaleY : Float;
	function new(?mapBitmap : flash.display.BitmapData, ?mapPoint : flash.geom.Point, componentX : UInt = 0, componentY : UInt = 0, scaleX : Float = 0, scaleY : Float = 0, ?mode : DisplacementMapFilterMode, color : UInt = 0, alpha : Float = 0) : Void;
}
