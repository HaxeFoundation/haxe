package flash.media;

extern class Microphone extends flash.events.EventDispatcher {
	function new() : Void;
	var activityLevel(default,null) : Float;
	var gain : Float;
	var index(default,null) : Int;
	var muted(default,null) : Bool;
	var name(default,null) : String;
	var rate : Int;
	function setLoopBack(?state : Bool) : Void;
	function setSilenceLevel(silenceLevel : Float, ?timeout : Int) : Void;
	function setUseEchoSuppression(useEchoSuppression : Bool) : Void;
	var silenceLevel(default,null) : Float;
	var silenceTimeout(default,null) : Int;
	var soundTransform : flash.media.SoundTransform;
	var useEchoSuppression(default,null) : Bool;
	static function getMicrophone(?index : Int) : flash.media.Microphone;
	static var names(default,null) : Array<Dynamic>;
}
