package flash.media;

extern class AVPlayState {
	@:flash.property var state(get,never) : Int;
	function new(state : UInt) : Void;
	private function get_state() : Int;
	static final BUFFERING : Int;
	static final EOF : Int;
	static final PAUSED : Int;
	static final PLAYING : Int;
	static final READY : Int;
	static final SUSPENDED : Int;
	static final TRICK_PLAY : Int;
	static final UNINITIALIZED : Int;
	static final UNRECOVERABLE_ERROR : Int;
}
