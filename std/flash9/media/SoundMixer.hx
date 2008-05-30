package flash.media;

extern class SoundMixer {
	static var bufferTime : Int;
	static var soundTransform : SoundTransform;
	static function areSoundsInaccessible() : Bool;
	static function computeSpectrum(outputArray : flash.utils.ByteArray, ?FFTMode : Bool, ?stretchFactor : Int) : Void;
	static function stopAll() : Void;
}
