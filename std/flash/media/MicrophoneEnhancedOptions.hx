package flash.media;

@:require(flash10_2) extern final class MicrophoneEnhancedOptions {
	var autoGain(get,set) : Bool;
	var echoPath(get,set) : Int;
	var isVoiceDetected(get,set) : Int;
	var mode(get,set) : MicrophoneEnhancedMode;
	var nonLinearProcessing(get,set) : Bool;
	function new() : Void;
	private function get_autoGain() : Bool;
	private function get_echoPath() : Int;
	private function get_isVoiceDetected() : Int;
	private function get_mode() : MicrophoneEnhancedMode;
	private function get_nonLinearProcessing() : Bool;
	private function set_autoGain(value : Bool) : Bool;
	private function set_echoPath(value : Int) : Int;
	private function set_isVoiceDetected(value : Int) : Int;
	private function set_mode(value : MicrophoneEnhancedMode) : MicrophoneEnhancedMode;
	private function set_nonLinearProcessing(value : Bool) : Bool;
}
