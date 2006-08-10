package flash.media;

extern class SoundLoaderContext {
	function new(?bufferTime : Float, ?checkPolicyFile : Bool) : Void;
	var bufferTime : Float;
	var checkPolicyFile : Bool;
}
