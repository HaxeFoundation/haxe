package flash.events;

extern class AudioOutputChangeEvent extends Event {
	@:flash.property var reason(get,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?reason : String) : Void;
	private function get_reason() : String;
	static final AUDIO_OUTPUT_CHANGE : String;
}
