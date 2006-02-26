package flash;

extern class NetStream
{
	var time : Float;
	var currentFps : Float;
	var bufferTime : Float;
	var bufferLength : Float;
	var liveDelay : Float;
	var bytesLoaded : Int;
	var bytesTotal : Int;

	function new( connection : NetConnection ) : Void;
	function onMetaData( info : Dynamic ) : Void;
	function onStatus( info : Dynamic ) : Void;
	function publish( name : Dynamic, type : String ) : Void;
	function play( name : Dynamic, start : Float, len : Float, reset : Dynamic ) : Void;
	function receiveAudio( flag : Bool ) : Void;
	function receiveVideo( flag : Dynamic ) : Void;
	function pause( flag : Bool ) : Void;
	function seek( offset : Float ) : Void;
	function close() : Void;
	function attachAudio( theMicrophone : Microphone ) : Void;
	function attachVideo( theCamera : Camera, snapshotMilliseconds : Float ) : Void;
	function send( handlerName : String ) : Void;
	function setBufferTime( bufferTime : Float ) : Void;
}


