package flash.events;

extern class AVStreamSwitchEvent extends Event {
	var bitrate(get,never) : Int;
	var description(get,never) : String;
	var switchType(get,never) : Int;
	var time(get,never) : Float;
	var userData(get,never) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, time : Float = 0, switchType : Int = 0, bitrate : Int = 0, ?description : String, userData : Int = 0) : Void;
	private function get_bitrate() : Int;
	private function get_description() : String;
	private function get_switchType() : Int;
	private function get_time() : Float;
	private function get_userData() : Int;
	static final ABR_SWITCH : Int;
	static final AV_STREAM_SWITCH : String;
	static final PERIOD_SWITCH : Int;
}
