package flash.media;

extern class Sound extends flash.events.EventDispatcher {
	@:flash.property var bytesLoaded(get,never) : UInt;
	@:flash.property var bytesTotal(get,never) : Int;
	@:flash.property var id3(get,never) : ID3Info;
	@:flash.property var isBuffering(get,never) : Bool;
	@:flash.property @:require(flash10_1) var isURLInaccessible(get,never) : Bool;
	@:flash.property var length(get,never) : Float;
	@:flash.property var url(get,never) : String;
	function new(?stream : flash.net.URLRequest, ?context : SoundLoaderContext) : Void;
	function close() : Void;
	@:require(flash10) function extract(target : flash.utils.ByteArray, length : Float, startPosition : Float = -1) : Float;
	private function get_bytesLoaded() : UInt;
	private function get_bytesTotal() : Int;
	private function get_id3() : ID3Info;
	private function get_isBuffering() : Bool;
	private function get_isURLInaccessible() : Bool;
	private function get_length() : Float;
	private function get_url() : String;
	function load(stream : flash.net.URLRequest, ?context : SoundLoaderContext) : Void;
	@:require(flash11) function loadCompressedDataFromByteArray(bytes : flash.utils.ByteArray, bytesLength : UInt) : Void;
	@:require(flash11) function loadPCMFromByteArray(bytes : flash.utils.ByteArray, samples : UInt, ?format : String, stereo : Bool = true, sampleRate : Float = 44100) : Void;
	function play(startTime : Float = 0, loops : Int = 0, ?sndTransform : SoundTransform) : SoundChannel;
}
