package flash.events;

extern class AVManifestLoadEvent extends Event {
	var duration(default,null) : Float;
	var handle(default,null) : Int;
	var result(default,null) : flash.media.AVResult;
	var userData(default,null) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, inResult : Int = 0, inUserData : Int = 0, inHandle : Int = 0, inDuration : Float = 0) : Void;
	static var AV_MANIFEST_LOAD : String;
}
