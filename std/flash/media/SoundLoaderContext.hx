package flash.media;

extern class SoundLoaderContext {
	var bufferTime : Float;
	var checkPolicyFile : Bool;
	function new(bufferTime : Float = 1000, checkPolicyFile : Bool = false) : Void;
}
