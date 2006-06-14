package flash;

extern class NetStream
{
	var bufferLength(default,null) : Float;
	var bufferTime(default,null) : Float;
	var bytesLoaded(default,null) : Int;
	var bytesTotal(default,null) : Int;
	var time(default,null) : Float;
	var currentFps(default,null) : Float;

	// not documented ?
	var liveDelay : Float;

	function new( connection : NetConnection ) : Void;
	function onMetaData( info : Dynamic ) : Void;
	function onStatus( info : Dynamic ) : Void;
	function publish( name : Dynamic, type : String ) : Void;
	function play( name : String, start : Float, len : Float, reset : Dynamic ) : Void;
	function receiveAudio( flag : Bool ) : Void;
	function receiveVideo( flag : Dynamic ) : Void;
	function pause( ?flag : Bool ) : Void;
	function seek( offset : Float ) : Void;
	function close() : Void;
	function attachAudio( theMicrophone : Microphone ) : Void;
	function attachVideo( theCamera : Camera, snapshotMilliseconds : Float ) : Void;
	function send( handlerName : String ) : Void;
	function setBufferTime( bufferTime : Float ) : Void;

#if flash8
	function onCuePoint( info : Dynamic ) : Void;
#end

	private static function __init__() : Void untyped {
		flash.NetStream = _global["NetStream"];
	}

}
