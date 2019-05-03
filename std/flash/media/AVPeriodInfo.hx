package flash.media;

extern class AVPeriodInfo {
	var duration(get,never) : Float;
	var firstCuePointIndex(get,never) : Int;
	var firstSubscribedTagIndex(get,never) : Int;
	var lastCuePointIndex(get,never) : Int;
	var lastSubscribedTagIndex(get,never) : Int;
	var localStartTime(get,never) : Float;
	var supportsTrickPlay(get,never) : Bool;
	var targetDuration(get,never) : Float;
	var userData(get,never) : Int;
	var virtualStartTime(get,never) : Float;
	function new(init_localStartTime : Float, init_virtualStartTime : Float, init_duration : Float, init_firstCuePointIndex : Int, init_lastCuePointIndex : Int, init_firstSubscribedTagIndex : Int, init_lastSubscribedTagIndex : Int, init_userData : Int, init_supportsTrickPlay : Bool, init_targetDuration : Float) : Void;
	private function get_duration() : Float;
	private function get_firstCuePointIndex() : Int;
	private function get_firstSubscribedTagIndex() : Int;
	private function get_lastCuePointIndex() : Int;
	private function get_lastSubscribedTagIndex() : Int;
	private function get_localStartTime() : Float;
	private function get_supportsTrickPlay() : Bool;
	private function get_targetDuration() : Float;
	private function get_userData() : Int;
	private function get_virtualStartTime() : Float;
}
