package flash.media;

extern class SoundLoaderContext {
	var bufferTime : Float;
	var checkPolicyFile : Bool;
	function new(?bufferTime : Float, ?checkPolicyFile : Bool) : Void;
}
