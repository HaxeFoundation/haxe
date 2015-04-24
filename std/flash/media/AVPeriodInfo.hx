package flash.media;

extern class AVPeriodInfo {
	var duration(default,null) : Float;
	var firstCuePointIndex(default,null) : Int;
	var firstSubscribedTagIndex(default,null) : Int;
	var lastCuePointIndex(default,null) : Int;
	var lastSubscribedTagIndex(default,null) : Int;
	var localStartTime(default,null) : Float;
	var supportsTrickPlay(default,null) : Bool;
	var targetDuration(default,null) : Float;
	var userData(default,null) : Int;
	var virtualStartTime(default,null) : Float;
	function new(init_localStartTime : Float, init_virtualStartTime : Float, init_duration : Float, init_firstCuePointIndex : Int, init_lastCuePointIndex : Int, init_firstSubscribedTagIndex : Int, init_lastSubscribedTagIndex : Int, init_userData : Int, init_supportsTrickPlay : Bool, init_targetDuration : Float) : Void;
}
