package flash.media;

extern class AVPlayState {
	var state(default,never) : Int;
	function new(state : UInt) : Void;
	static var BUFFERING(default,never) : Int;
	static var EOF(default,never) : Int;
	static var PAUSED(default,never) : Int;
	static var PLAYING(default,never) : Int;
	static var READY(default,never) : Int;
	static var SUSPENDED(default,never) : Int;
	static var TRICK_PLAY(default,never) : Int;
	static var UNINITIALIZED(default,never) : Int;
	static var UNRECOVERABLE_ERROR(default,never) : Int;
}
