package flash.system;

extern class Capabilities {
	static var _internal(default,never) : UInt;
	static var avHardwareDisable(default,never) : Bool;
	@:require(flash10_1) static var cpuArchitecture(default,never) : String;
	static var hasAccessibility(default,never) : Bool;
	static var hasAudio(default,never) : Bool;
	static var hasAudioEncoder(default,never) : Bool;
	static var hasEmbeddedVideo(default,never) : Bool;
	static var hasIME(default,never) : Bool;
	static var hasMP3(default,never) : Bool;
	static var hasPrinting(default,never) : Bool;
	static var hasScreenBroadcast(default,never) : Bool;
	static var hasScreenPlayback(default,never) : Bool;
	static var hasStreamingAudio(default,never) : Bool;
	static var hasStreamingVideo(default,never) : Bool;
	static var hasTLS(default,never) : Bool;
	static var hasVideoEncoder(default,never) : Bool;
	static var isDebugger(default,never) : Bool;
	@:require(flash10) static var isEmbeddedInAcrobat(default,never) : Bool;
	static var language(default,never) : String;
	static var localFileReadDisable(default,never) : Bool;
	static var manufacturer(default,never) : String;
	@:require(flash10) static var maxLevelIDC(default,never) : String;
	static var os(default,never) : String;
	static var pixelAspectRatio(default,never) : Float;
	static var playerType(default,never) : String;
	static var screenColor(default,never) : String;
	static var screenDPI(default,never) : Float;
	static var screenResolutionX(default,never) : Float;
	static var screenResolutionY(default,never) : Float;
	static var serverString(default,never) : String;
	@:require(flash10_1) static var supports32BitProcesses(default,never) : Bool;
	@:require(flash10_1) static var supports64BitProcesses(default,never) : Bool;
	@:require(flash10_1) static var touchscreenType(default,never) : TouchscreenType;
	static var version(default,never) : String;
	@:require(flash11) static function hasMultiChannelAudio(type : String) : Bool;
}
