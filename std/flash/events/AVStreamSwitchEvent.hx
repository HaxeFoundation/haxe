package flash.events;

extern class AVStreamSwitchEvent extends Event {
	var bitrate(default,null) : Int;
	var switchType(default,null) : Int;
	var time(default,null) : Float;
	var userData(default,null) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, time : Float = 0, switchType : Int = 0, bitrate : Int = 0, userData : Int = 0) : Void;
	static var ABR_SWITCH : Int;
	static var AV_STREAM_SWITCH : String;
	static var PERIOD_SWITCH : Int;
}
