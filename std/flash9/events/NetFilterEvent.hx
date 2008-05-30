package flash.events;

extern class NetFilterEvent extends Event {
	var data : flash.utils.ByteArray;
	var header : flash.utils.ByteArray;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?header : flash.utils.ByteArray, ?data : flash.utils.ByteArray) : Void;
}
