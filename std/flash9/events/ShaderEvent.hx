package flash.events;

extern class ShaderEvent extends Event  {
	var byteArray : flash.utils.ByteArray;
	var vector : flash.Vector<Float>;
	var bitmapData : flash.display.BitmapData;

	function new( ?type : String, ?bubbles : Bool, ?cancelable : Bool, ?bitmap : flash.display.BitmapData, ?array : flash.utils.ByteArray, ?vector : flash.Vector<Float> ) : Void;

	public static var COMPLETE(default,null) : String;
}
