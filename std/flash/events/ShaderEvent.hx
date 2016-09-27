package flash.events;

extern class ShaderEvent extends Event {
	var bitmapData : flash.display.BitmapData;
	var byteArray : flash.utils.ByteArray;
	var vector : flash.Vector<Float>;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?bitmap : flash.display.BitmapData, ?array : flash.utils.ByteArray, ?vector : flash.Vector<Float>) : Void;
	static var COMPLETE(default,never) : String;
}
