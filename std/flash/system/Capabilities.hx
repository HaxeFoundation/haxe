package flash.system;

extern class Capabilities {
	static var _internal(default,null) : UInt;
	static var avHardwareDisable(default,null) : Bool;
	@:require(flash10_1) static var cpuArchitecture(default,null) : String;
	static var hasAccessibility(default,null) : Bool;
	static var hasAudio(default,null) : Bool;
	static var hasAudioEncoder(default,null) : Bool;
	static var hasEmbeddedVideo(default,null) : Bool;
	static var hasIME(default,null) : Bool;
	static var hasMP3(default,null) : Bool;
	static var hasPrinting(default,null) : Bool;
	static var hasScreenBroadcast(default,null) : Bool;
	static var hasScreenPlayback(default,null) : Bool;
	static var hasStreamingAudio(default,null) : Bool;
	static var hasStreamingVideo(default,null) : Bool;
	static var hasTLS(default,null) : Bool;
	static var hasVideoEncoder(default,null) : Bool;
	static var isDebugger(default,null) : Bool;
	@:require(flash10) static var isEmbeddedInAcrobat(default,null) : Bool;
	static var language(default,null) : String;
	static var localFileReadDisable(default,null) : Bool;
	static var manufacturer(default,null) : String;
	@:require(flash10) static var maxLevelIDC(default,null) : String;
	static var os(default,null) : String;
	static var pixelAspectRatio(default,null) : Float;
	static var playerType(default,null) : String;
	static var screenColor(default,null) : String;
	static var screenDPI(default,null) : Float;
	static var screenResolutionX(default,null) : Float;
	static var screenResolutionY(default,null) : Float;
	static var serverString(default,null) : String;
	@:require(flash10_1) static var supports32BitProcesses(default,null) : Bool;
	@:require(flash10_1) static var supports64BitProcesses(default,null) : Bool;
	@:require(flash10_1) static var touchscreenType(default,null) : TouchscreenType;
	static var version(default,null) : String;
	@:require(flash11) static function hasMultiChannelAudio(type : String) : Bool;
}
