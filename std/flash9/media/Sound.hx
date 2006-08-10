package flash.media;

extern class Sound extends flash.events.EventDispatcher {
	function new(?stream : flash.net.URLRequest, ?context : flash.media.SoundLoaderContext) : Void;
	var bytesLoaded(default,null) : UInt;
	var bytesTotal(default,null) : Int;
	function close() : Void;
	var id3(default,null) : flash.media.ID3Info;
	var isBuffering(default,null) : Bool;
	var length(default,null) : Float;
	function load(stream : flash.net.URLRequest, ?context : flash.media.SoundLoaderContext) : Void;
	function play(?startTime : Float, ?loops : Int, ?sndTransform : flash.media.SoundTransform) : flash.media.SoundChannel;
	var url(default,null) : String;
	private function _buildLoaderContext(context : flash.media.SoundLoaderContext) : flash.media.SoundLoaderContext;
	private function _load(stream : flash.net.URLRequest, checkPolicyFile : Bool, bufferTime : Float) : Void;
}
