package flash.events;

extern class AVStreamSwitchEvent extends Event {
	var bitrate(default,null) : Int;
	var description(default,null) : String;
	var switchType(default,null) : Int;
	var time(default,null) : Float;
	var userData(default,null) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, time : Float = 0, switchType : Int = 0, bitrate : Int = 0, ?description : String, userData : Int = 0) : Void;
	static var ABR_SWITCH(default,never) : Int;
	static var AV_STREAM_SWITCH(default,never) : String;
	static var PERIOD_SWITCH(default,never) : Int;
}
