package flash.media;

extern class Microphone extends flash.events.EventDispatcher {
	var activityLevel(default,null) : Float;
	var gain : Float;
	var index(default,null) : Int;
	var muted(default,null) : Bool;
	var name(default,null) : String;
	var rate : Int;
	var silenceLevel(default,null) : Float;
	var silenceTimeout(default,null) : Int;
	var soundTransform : SoundTransform;
	var useEchoSuppression(default,null) : Bool;
	#if flash10
	var codec : SoundCodec;
	var encodeQuality : Int;
	var framesPerPacket : Int;
	#end

	function new() : Void;
	function setLoopBack(?state : Bool) : Void;
	function setSilenceLevel(silenceLevel : Float, ?timeout : Int) : Void;
	function setUseEchoSuppression(useEchoSuppression : Bool) : Void;
	static var names(default,null) : Array<Dynamic>;
	static function getMicrophone(?index : Int) : Microphone;
}
