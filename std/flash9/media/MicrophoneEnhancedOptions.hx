package flash.media;

@:final @:require(flash10_2) extern class MicrophoneEnhancedOptions {
	var autoGain : Bool;
	var echoPath : Int;
	var isVoiceDetected(default,null) : Int;
	var mode : MicrophoneEnhancedMode;
	var nonLinearProcessing : Bool;
	function new() : Void;
}
