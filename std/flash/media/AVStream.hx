package flash.media;

extern class AVStream extends flash.events.EventDispatcher {
	var backBufferLength(default,null) : Float;
	var backBufferTime(null,default) : Float;
	var bufferLength(default,null) : Float;
	var bufferTime(null,default) : Float;
	var captionStyle(null,default) : AVCaptionStyle;
	var captionsEnabled : Bool;
	var currentFPS(default,null) : Float;
	var decoderType(default,null) : String;
	var droppedFrames(default,null) : Int;
	var frameTime(default,null) : Float;
	var initialBufferTime(null,default) : Float;
	var playState(default,null) : AVPlayState;
	var renderType(default,null) : String;
	var time(default,null) : Float;
	var useHardwareDecoder : Bool;
	var volume : Float;
	function new(source : AVSource) : Void;
	function dispose() : Void;
	function pause() : AVResult;
	function play() : AVResult;
	function resume() : Bool;
	function seek(offset : Float, inBufferSeek : Bool = true) : AVResult;
	function seekToLivePoint() : AVResult;
	function seekToLocalTime(periodIndex : Int, time : Float) : AVResult;
	function step(frames : Int) : AVResult;
	static var HARDWARE : String;
	static var SOFTWARE : String;
	static var UNDEFINED : String;
}
