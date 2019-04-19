package flash.events;

extern class AVStreamSwitchEvent extends Event {
	var bitrate(default,never) : Int;
	var description(default,never) : String;
	var switchType(default,never) : Int;
	var time(default,never) : Float;
	var userData(default,never) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, time : Float = 0, switchType : Int = 0, bitrate : Int = 0, ?description : String, userData : Int = 0) : Void;
	static final ABR_SWITCH : Int;
	static final AV_STREAM_SWITCH : String;
	static final PERIOD_SWITCH : Int;
}
