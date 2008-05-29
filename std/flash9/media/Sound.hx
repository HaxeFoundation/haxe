package flash.media;

extern class Sound extends flash.events.EventDispatcher {

	var bytesLoaded(default,null) : UInt;
	var bytesTotal(default,null) : Int;
	var id3(default,null) : flash.media.ID3Info;
	var isBuffering(default,null) : Bool;
	var length(default,null) : Float;
	var url(default,null) : String;

	function new(?stream : flash.net.URLRequest, ?context : flash.media.SoundLoaderContext) : Void;
	function close() : Void;
	function load(stream : flash.net.URLRequest, ?context : flash.media.SoundLoaderContext) : Void;
	function play(?startTime : Float, ?loops : Int, ?sndTransform : flash.media.SoundTransform) : flash.media.SoundChannel;

	#if flash10
	var samplesCallbackData(default,null) : flash.utils.ByteArray;
	function extract( ?target : flash.utils.ByteArray, ?length : Float, ?startPosition : Float ) : Float;
	#end
}
