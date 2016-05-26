package flash.media;

extern class AVTimeline {
	var complete(default,never) : Bool;
	var firstPeriodIndex(default,never) : Int;
	var firstSubscribedTagIndex(default,never) : Int;
	var lastPeriodIndex(default,never) : Int;
	var lastSubscribedTagIndex(default,never) : Int;
	var type(default,never) : String;
	var virtualDuration(default,never) : Float;
	var virtualStartTime(default,never) : Float;
	function new(init_type : String, init_virtualStartTime : Float, init_virtualDuration : Float, init_firstPeriodIndex : Int, init_lastPeriodIndex : Int, init_firstSubscribedIndex : Int, init_lastSubscribedIndex : Int, init_complete : Bool) : Void;
}
