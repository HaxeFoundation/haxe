package flash.media;

extern class AVTimeline {
	@:flash.property var complete(get,never) : Bool;
	@:flash.property var firstPeriodIndex(get,never) : Int;
	@:flash.property var firstSubscribedTagIndex(get,never) : Int;
	@:flash.property var lastPeriodIndex(get,never) : Int;
	@:flash.property var lastSubscribedTagIndex(get,never) : Int;
	@:flash.property var type(get,never) : String;
	@:flash.property var virtualDuration(get,never) : Float;
	@:flash.property var virtualStartTime(get,never) : Float;
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
