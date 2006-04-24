package flash;

extern class NetStream
{
	property bufferLength(default,null) : Float;
	property bufferTime(default,null) : Float;
	property bytesLoaded(default,null) : Int;
	property bytesTotal(default,null) : Int;
	property time(default,null) : Float;
	property currentFps(default,null) : Float;

	// not documented ?
	var liveDelay : Float;

	function new( connection : NetConnection ) : Void;
	function onMetaData( info : Dynamic ) : Void;
	function onStatus( info : Dynamic ) : Void;
	function publish( name : Dynamic, type : String ) : Void;
	function play( name : String, start : Float, len : Float, reset : Dynamic ) : Void;
	function receiveAudio( flag : Bool ) : Void;
	function receiveVideo( flag : Dynamic ) : Void;
	function pause( flag : Bool ) : Void;
	function seek( offset : Float ) : Void;
	function close() : Void;
	function attachAudio( theMicrophone : Microphone ) : Void;
	function attachVideo( theCamera : Camera, snapshotMilliseconds : Float ) : Void;
	function send( handlerName : String ) : Void;
	function setBufferTime( bufferTime : Float ) : Void;

#if flash8
	function onCuePoint( info : Dynamic ) : Void;
#end

}


