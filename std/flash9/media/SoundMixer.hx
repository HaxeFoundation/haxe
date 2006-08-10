package flash.media;

extern class SoundMixer {
	function new() : Void;
	static function areSoundsInaccessible() : Bool;
	static var bufferTime : Int;
	static function computeSpectrum(outputArray : flash.utils.ByteArray, ?FFTMode : Bool, ?stretchFactor : Int) : Void;
	static var soundTransform : flash.media.SoundTransform;
	static function stopAll() : Void;
}
