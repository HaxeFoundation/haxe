package flash.media;

extern class AVTimeline {
	var complete(get,never) : Bool;
	var firstPeriodIndex(get,never) : Int;
	var firstSubscribedTagIndex(get,never) : Int;
	var lastPeriodIndex(get,never) : Int;
	var lastSubscribedTagIndex(get,never) : Int;
	var type(get,never) : String;
	var virtualDuration(get,never) : Float;
	var virtualStartTime(get,never) : Float;
	function new(init_type : String, init_virtualStartTime : Float, init_virtualDuration : Float, init_firstPeriodIndex : Int, init_lastPeriodIndex : Int, init_firstSubscribedIndex : Int, init_lastSubscribedIndex : Int, init_complete : Bool) : Void;
	private function get_complete() : Bool;
	private function get_firstPeriodIndex() : Int;
	private function get_firstSubscribedTagIndex() : Int;
	private function get_lastPeriodIndex() : Int;
	private function get_lastSubscribedTagIndex() : Int;
	private function get_type() : String;
	private function get_virtualDuration() : Float;
	private function get_virtualStartTime() : Float;
}
