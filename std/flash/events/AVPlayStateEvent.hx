package flash.events;

extern class AVPlayStateEvent extends Event {
	@:flash.property var playState(get,never) : flash.media.AVPlayState;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, inState : Int = 0) : Void;
	private function get_playState() : flash.media.AVPlayState;
	static final AV_PLAY_STATE : String;
}
