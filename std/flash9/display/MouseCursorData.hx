package flash.display;

@:final @:require(flash10_2) extern class MouseCursorData {
	var data : flash.Vector<BitmapData>;
	var frameRate : Float;
	var hotSpot : flash.geom.Point;
	var name : String;
	function new(name : String) : Void;
}
