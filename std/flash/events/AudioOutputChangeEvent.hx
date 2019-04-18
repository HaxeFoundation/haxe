package flash.events;

extern class AudioOutputChangeEvent extends Event {
	var reason(default,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?reason : String) : Void;
	static final AUDIO_OUTPUT_CHANGE : String;
}
