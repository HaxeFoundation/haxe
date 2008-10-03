package flash.events;

extern class SampleDataEvent extends Event {
	var position : Float;
	var data : flash.utils.ByteArray;
    function new( ?type : String, ?bubbles : Bool, ?cancelable : Bool, ?position : Float, ?data : flash.utils.ByteArray ) : Void;
    static var SAMPLE_DATA : String;
}
