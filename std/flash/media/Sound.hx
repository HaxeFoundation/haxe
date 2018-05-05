package flash.media;

extern class Sound extends flash.events.EventDispatcher {
	var bytesLoaded(default,never) : UInt;
	var bytesTotal(default,never) : Int;
	var id3(default,never) : ID3Info;
	var isBuffering(default,never) : Bool;
	@:require(flash10_1) var isURLInaccessible(default,never) : Bool;
	var length(default,never) : Float;
	var url(default,never) : String;
	function new(?stream : flash.net.URLRequest, ?context : SoundLoaderContext) : Void;
	function close() : Void;
	@:require(flash10) function extract(target : flash.utils.ByteArray, length : Float, startPosition : Float = -1) : Float;
	function load(stream : flash.net.URLRequest, ?context : SoundLoaderContext) : Void;
	@:require(flash11) function loadCompressedDataFromByteArray(bytes : flash.utils.ByteArray, bytesLength : UInt) : Void;
	@:require(flash11) function loadPCMFromByteArray(bytes : flash.utils.ByteArray, samples : UInt, ?format : String, stereo : Bool = true, sampleRate : Float = 44100) : Void;
	function play(startTime : Float = 0, loops : Int = 0, ?sndTransform : SoundTransform) : SoundChannel;
}
