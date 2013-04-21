package flash.media;

extern class AVPlayState {
	var state(default,null) : Int;
	function new(state : UInt) : Void;
	static var BUFFERING : Int;
	static var EOF : Int;
	static var PAUSED : Int;
	static var PLAYING : Int;
	static var READY : Int;
	static var SUSPENDED : Int;
	static var UNINITIALIZED : Int;
	static var UNRECOVERABLE_ERROR : Int;
}
