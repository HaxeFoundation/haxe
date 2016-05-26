package flash.media;

extern class AVPeriodInfo {
	var duration(default,never) : Float;
	var firstCuePointIndex(default,never) : Int;
	var firstSubscribedTagIndex(default,never) : Int;
	var lastCuePointIndex(default,never) : Int;
	var lastSubscribedTagIndex(default,never) : Int;
	var localStartTime(default,never) : Float;
	var supportsTrickPlay(default,never) : Bool;
	var targetDuration(default,never) : Float;
	var userData(default,never) : Int;
	var virtualStartTime(default,never) : Float;
	function new(init_localStartTime : Float, init_virtualStartTime : Float, init_duration : Float, init_firstCuePointIndex : Int, init_lastCuePointIndex : Int, init_firstSubscribedTagIndex : Int, init_lastSubscribedTagIndex : Int, init_userData : Int, init_supportsTrickPlay : Bool, init_targetDuration : Float) : Void;
}
