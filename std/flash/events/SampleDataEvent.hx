package flash.events;

extern class SampleDataEvent extends Event {
	var data : flash.utils.ByteArray;
	var position : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, theposition : Float = 0, ?thedata : flash.utils.ByteArray) : Void;
	static var SAMPLE_DATA(default,never) : String;
}
