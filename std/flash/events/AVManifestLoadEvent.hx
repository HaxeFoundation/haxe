package flash.events;

extern class AVManifestLoadEvent extends Event {
	var duration(default,never) : Float;
	var handle(default,never) : Int;
	var result(default,never) : flash.media.AVResult;
	var userData(default,never) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, inResult : Int = 0, inUserData : Int = 0, inHandle : Int = 0, inDuration : Float = 0) : Void;
	static var AV_MANIFEST_LOAD(default,never) : String;
}
