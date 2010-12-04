package flash.net;

@:final extern class NetStreamInfo {
	var SRTT(default,null) : Float;
	var audioBufferByteLength(default,null) : Float;
	var audioBufferLength(default,null) : Float;
	var audioByteCount(default,null) : Float;
	var audioBytesPerSecond(default,null) : Float;
	var audioLossRate(default,null) : Float;
	var byteCount(default,null) : Float;
	var currentBytesPerSecond(default,null) : Float;
	var dataBufferByteLength(default,null) : Float;
	var dataBufferLength(default,null) : Float;
	var dataByteCount(default,null) : Float;
	var dataBytesPerSecond(default,null) : Float;
	var droppedFrames(default,null) : Float;
	var maxBytesPerSecond(default,null) : Float;
	var playbackBytesPerSecond(default,null) : Float;
	var videoBufferByteLength(default,null) : Float;
	var videoBufferLength(default,null) : Float;
	var videoByteCount(default,null) : Float;
	var videoBytesPerSecond(default,null) : Float;
	function new(curBPS : Float, byteCount : Float, maxBPS : Float, audioBPS : Float, audioByteCount : Float, videoBPS : Float, videoByteCount : Float, dataBPS : Float, dataByteCount : Float, playbackBPS : Float, droppedFrames : Float, audioBufferByteLength : Float, videoBufferByteLength : Float, dataBufferByteLength : Float, audioBufferLength : Float, videoBufferLength : Float, dataBufferLength : Float, srtt : Float, audioLossRate : Float) : Void;
	function toString() : String;
}
