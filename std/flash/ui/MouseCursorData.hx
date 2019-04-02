package flash.ui;

@:require(flash10_2) extern final class MouseCursorData {
	var data : flash.Vector<flash.display.BitmapData>;
	var frameRate : Float;
	var hotSpot : flash.geom.Point;
	function new() : Void;
}
