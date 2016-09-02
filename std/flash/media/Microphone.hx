package flash.media;

@:final extern class Microphone extends flash.events.EventDispatcher {
	var activityLevel(default,never) : Float;
	@:require(flash10) var codec : SoundCodec;
	@:require(flash10_1) var enableVAD : Bool;
	@:require(flash10) var encodeQuality : Int;
	@:require(flash10_2) var enhancedOptions : MicrophoneEnhancedOptions;
	@:require(flash10) var framesPerPacket : Int;
	var gain : Float;
	var index(default,never) : Int;
	var muted(default,never) : Bool;
	var name(default,never) : String;
	@:require(flash10_1) var noiseSuppressionLevel : Int;
	var rate : Int;
	var silenceLevel(default,never) : Float;
	var silenceTimeout(default,never) : Int;
	var soundTransform : SoundTransform;
	var useEchoSuppression(default,never) : Bool;
	function new() : Void;
	function setLoopBack(state : Bool = true) : Void;
	function setSilenceLevel(silenceLevel : Float, timeout : Int = -1) : Void;
	function setUseEchoSuppression(useEchoSuppression : Bool) : Void;
	@:require(flash10_1) static var isSupported(default,never) : Bool;
	static var names(default,never) : Array<Dynamic>;
	@:require(flash10_2) static function getEnhancedMicrophone(index : Int = -1) : Microphone;
	static function getMicrophone(index : Int = -1) : Microphone;
}
