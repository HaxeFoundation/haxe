package flash.media;

extern class AVStream extends flash.events.EventDispatcher {
	var backBufferLength(default,never) : Float;
	var backBufferTime(never,default) : Float;
	var bufferLength(default,never) : Float;
	var bufferTime(never,default) : Float;
	var captionStyle(never,default) : AVCaptionStyle;
	var captionsEnabled : Bool;
	var clientLivePoint(default,never) : Float;
	var currentFPS(default,never) : Float;
	var decoderType(default,never) : String;
	var droppedFrames(default,never) : Int;
	var frameTime(default,never) : Float;
	var initialBufferTime(never,default) : Float;
	var playState(default,never) : AVPlayState;
	var renderType(default,never) : String;
	var time(default,never) : Float;
	var useHardwareDecoder : Bool;
	var volume : Float;
	function new(source : AVSource) : Void;
	function dispose() : Void;
	function fastForward(rate : Float) : AVResult;
	function pause() : AVResult;
	function play() : AVResult;
	function resume() : Bool;
	function rewind(rate : Float) : AVResult;
	function seek(offset : Float, inBufferSeek : Bool = true) : AVResult;
	function seekToKeyFrame(offset : Float, inBufferSeek : Bool = true) : AVResult;
	function seekToLivePoint() : AVResult;
	function seekToLocalTime(periodIndex : Int, time : Float) : AVResult;
	function setPlaySpeed(speed : Float, reserved : Float) : Void;
	function step(frames : Int) : AVResult;
	static var HARDWARE(default,never) : String;
	static var SOFTWARE(default,never) : String;
	static var UNDEFINED(default,never) : String;
}
