package flash.media;

@:require(flash10_2) extern final class MicrophoneEnhancedOptions {
	var autoGain : Bool;
	var echoPath : Int;
	var isVoiceDetected : Int;
	var mode : MicrophoneEnhancedMode;
	var nonLinearProcessing : Bool;
	function new() : Void;
}
