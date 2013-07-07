package flash.system;

extern class Capabilities
{
	static var hasAudio:Bool;
	static var hasMP3:Bool;
	static var hasAudioEncoder:Bool;
	static var hasVideoEncoder:Bool;
	static var screenResolutionX:Float;
	static var screenResolutionY:Float;
	static var screenDPI:Float;
	static var screenColor:String;
	static var pixelAspectRatio:Float;
	static var hasAccessibility:Bool;
	static var input:String;
	static var isDebugger:Bool;
	static var language:String;
	static var manufacturer:String;
	static var os:String;
	static var serverString:String;
	static var version:String;
	static var hasPrinting:Bool;
	static var playerType:String;
	static var hasStreamingAudio:Bool;
	static var hasScreenBroadcast:Bool;
	static var hasScreenPlayback:Bool;
	static var hasStreamingVideo:Bool;
	static var hasEmbeddedVideo:Bool;
	static var avHardwareDisable:Bool;
	static var localFileReadDisable:Bool;
	static var windowlessDisable:Bool;

	#if flash_lite
	static var hasCompoundSound:Bool;
	static var hasEmail:Bool;
	static var hasMMS:Bool;
	static var hasSMS:Bool;
	static var hasMFI:Bool;
	static var hasMIDI:Bool;
	static var hasSMAF:Bool;
	static var hasDataLoading:Bool;
	static var has4WayKeyAS:Bool;
	static var hasMouse:Bool;
	static var hasMappableSoftKeys:Bool;
	static var hasCMIDI:Bool;
	static var hasStylus:Bool;
	static var screenOrientation:String;
	static var hasSharedObjects:Bool;
	static var hasQWERTYKeyboard:Bool;
	static var softKeyCount:Float;
	static var audioMIMETypes:Array<String>;
	static var imageMIMETypes:Array<String>;
	static var videoMIMETypes:Array<String>;
	static var MIMETypes:String;
	#end

	private static function __init__() : Void untyped {
		flash.system.Capabilities = _global["System"]["capabilities"];
	}

}
