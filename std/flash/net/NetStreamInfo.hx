package flash.net;

@:final extern class NetStreamInfo {
	var SRTT(default,never) : Float;
	var audioBufferByteLength(default,never) : Float;
	var audioBufferLength(default,never) : Float;
	var audioByteCount(default,never) : Float;
	var audioBytesPerSecond(default,never) : Float;
	var audioLossRate(default,never) : Float;
	var byteCount(default,never) : Float;
	var currentBytesPerSecond(default,never) : Float;
	var dataBufferByteLength(default,never) : Float;
	var dataBufferLength(default,never) : Float;
	var dataByteCount(default,never) : Float;
	var dataBytesPerSecond(default,never) : Float;
	var droppedFrames(default,never) : Float;
	var isLive(default,never) : Bool;
	var maxBytesPerSecond(default,never) : Float;
	var metaData(default,never) : Dynamic;
	var playbackBytesPerSecond(default,never) : Float;
	var resourceName(default,never) : String;
	var uri(default,never) : String;
	var videoBufferByteLength(default,never) : Float;
	var videoBufferLength(default,never) : Float;
	var videoByteCount(default,never) : Float;
	var videoBytesPerSecond(default,never) : Float;
	@:require(flash10_1) var videoLossRate(default,never) : Float;
	var xmpData(default,never) : Dynamic;
	function new(curBPS : Float, byteCount : Float, maxBPS : Float, audioBPS : Float, audioByteCount : Float, videoBPS : Float, videoByteCount : Float, dataBPS : Float, dataByteCount : Float, playbackBPS : Float, droppedFrames : Float, audioBufferByteLength : Float, videoBufferByteLength : Float, dataBufferByteLength : Float, audioBufferLength : Float, videoBufferLength : Float, dataBufferLength : Float, srtt : Float, audioLossRate : Float, videoLossRate : Float, ?metaData : Dynamic, ?xmpData : Dynamic, ?uri : String, ?resourceName : String, isLive : Bool = true) : Void;
	function toString() : String;
}
