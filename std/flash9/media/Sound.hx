package flash.media;

extern class Sound extends flash.events.EventDispatcher {
	var bytesLoaded(default,null) : UInt;
	var bytesTotal(default,null) : Int;
	var id3(default,null) : ID3Info;
	var isBuffering(default,null) : Bool;
	var length(default,null) : Float;
	var url(default,null) : String;
	function new(?stream : flash.net.URLRequest, ?context : SoundLoaderContext) : Void;
	function close() : Void;
	function load(stream : flash.net.URLRequest, ?context : SoundLoaderContext) : Void;
	function play(startTime : Float = 0, loops : Int = 0, ?sndTransform : SoundTransform) : SoundChannel;
}
