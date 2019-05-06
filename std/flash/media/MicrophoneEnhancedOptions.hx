package flash.media;

@:require(flash10_2) extern final class MicrophoneEnhancedOptions {
	@:flash.property var autoGain(get,set) : Bool;
	@:flash.property var echoPath(get,set) : Int;
	@:flash.property var isVoiceDetected(get,set) : Int;
	@:flash.property var mode(get,set) : MicrophoneEnhancedMode;
	@:flash.property var nonLinearProcessing(get,set) : Bool;
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
