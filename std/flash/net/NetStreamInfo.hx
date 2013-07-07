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
	var isLive(default,null) : Bool;
	var maxBytesPerSecond(default,null) : Float;
	var metaData(default,null) : Dynamic;
	var playbackBytesPerSecond(default,null) : Float;
	var resourceName(default,null) : String;
	var uri(default,null) : String;
	var videoBufferByteLength(default,null) : Float;
	var videoBufferLength(default,null) : Float;
	var videoByteCount(default,null) : Float;
	var videoBytesPerSecond(default,null) : Float;
	@:require(flash10_1) var videoLossRate(default,null) : Float;
	var xmpData(default,null) : Dynamic;
	function new(curBPS : Float, byteCount : Float, maxBPS : Float, audioBPS : Float, audioByteCount : Float, videoBPS : Float, videoByteCount : Float, dataBPS : Float, dataByteCount : Float, playbackBPS : Float, droppedFrames : Float, audioBufferByteLength : Float, videoBufferByteLength : Float, dataBufferByteLength : Float, audioBufferLength : Float, videoBufferLength : Float, dataBufferLength : Float, srtt : Float, audioLossRate : Float, videoLossRate : Float, ?metaData : Dynamic, ?xmpData : Dynamic, ?uri : String, ?resourceName : String, isLive : Bool = true) : Void;
	function toString() : String;
}
