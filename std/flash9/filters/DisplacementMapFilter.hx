package flash.filters;

extern class DisplacementMapFilter extends flash.filters.BitmapFilter {
	function new(?mapBitmap : flash.display.BitmapData, ?mapPoint : flash.geom.Point, ?componentX : UInt, ?componentY : UInt, ?scaleX : Float, ?scaleY : Float, ?mode : String, ?color : UInt, ?alpha : Float) : Void;
	var alpha : Float;
	var color : UInt;
	var componentX : UInt;
	var componentY : UInt;
	var mapBitmap : flash.display.BitmapData;
	var mapPoint : flash.geom.Point;
	var mode : String;
	var scaleX : Float;
	var scaleY : Float;
}
