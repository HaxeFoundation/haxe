package flash.media;

@:native("flash.media.MicrophoneEnhancedMode") @:require(flash10_2) extern enum abstract MicrophoneEnhancedMode(String) {
	var FULL_DUPLEX;
	var HALF_DUPLEX;
	var HEADSET;
	var OFF;
	var SPEAKER_MUTE;
}
