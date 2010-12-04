package flash.net;

extern class NetStream extends flash.events.EventDispatcher {
	var audioCodec(default,null) : UInt;
	var bufferLength(default,null) : Float;
	var bufferTime : Float;
	var bytesLoaded(default,null) : UInt;
	var bytesTotal(default,null) : UInt;
	var checkPolicyFile : Bool;
	var client : Dynamic;
	var currentFPS(default,null) : Float;
	var decodedFrames(default,null) : UInt;
	var liveDelay(default,null) : Float;
	var objectEncoding(default,null) : UInt;
	var soundTransform : flash.media.SoundTransform;
	var time(default,null) : Float;
	var videoCodec(default,null) : UInt;
	function new(connection : NetConnection) : Void;
	function attachAudio(microphone : flash.media.Microphone) : Void;
	function attachCamera(theCamera : flash.media.Camera, snapshotMilliseconds : Int = -1) : Void;
	function close() : Void;
	function pause() : Void;
	function play(?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	function publish(?name : String, ?type : String) : Void;
	function receiveAudio(flag : Bool) : Void;
	function receiveVideo(flag : Bool) : Void;
	function receiveVideoFPS(FPS : Float) : Void;
	function resume() : Void;
	function seek(offset : Float) : Void;
	function send(handlerName : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	function togglePause() : Void;
}
