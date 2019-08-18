package flash.media;

extern class SoundMixer {
	@:flash.property static var audioPlaybackMode(get,set) : String;
	@:flash.property static var bufferTime(get,set) : Int;
	@:flash.property static var soundTransform(get,set) : SoundTransform;
	@:flash.property static var useSpeakerphoneForVoice(get,set) : Bool;
	static function areSoundsInaccessible() : Bool;
	static function computeSpectrum(outputArray : flash.utils.ByteArray, FFTMode : Bool = false, stretchFactor : Int = 0) : Void;
	private static function get_audioPlaybackMode() : String;
	private static function get_bufferTime() : Int;
	private static function get_soundTransform() : SoundTransform;
	private static function get_useSpeakerphoneForVoice() : Bool;
	private static function set_audioPlaybackMode(value : String) : String;
	private static function set_bufferTime(value : Int) : Int;
	private static function set_soundTransform(value : SoundTransform) : SoundTransform;
	private static function set_useSpeakerphoneForVoice(value : Bool) : Bool;
	static function stopAll() : Void;
}
