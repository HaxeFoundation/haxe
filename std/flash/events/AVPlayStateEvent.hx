package flash.events;

extern class AVPlayStateEvent extends Event {
	var playState(default,never) : flash.media.AVPlayState;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, inState : Int = 0) : Void;
	static var AV_PLAY_STATE(default,never) : String;
}
