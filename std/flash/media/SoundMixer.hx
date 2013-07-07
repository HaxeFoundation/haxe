package flash.media;

extern class SoundMixer {
	static var audioPlaybackMode : String;
	static var bufferTime : Int;
	static var soundTransform : SoundTransform;
	static var useSpeakerphoneForVoice : Bool;
	static function areSoundsInaccessible() : Bool;
	static function computeSpectrum(outputArray : flash.utils.ByteArray, FFTMode : Bool = false, stretchFactor : Int = 0) : Void;
	static function stopAll() : Void;
}
