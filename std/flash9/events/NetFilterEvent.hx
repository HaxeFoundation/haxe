package flash.events;

extern class NetFilterEvent extends Event {
	var data : flash.utils.ByteArray;
	var header : flash.utils.ByteArray;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?header : flash.utils.ByteArray, ?data : flash.utils.ByteArray) : Void;
}
