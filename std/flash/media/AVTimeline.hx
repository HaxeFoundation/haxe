package flash.media;

extern class AVTimeline {
	var complete(default,null) : Bool;
	var firstPeriodIndex(default,null) : Int;
	var lastPeriodIndex(default,null) : Int;
	var type(default,null) : String;
	var virtualDuration(default,null) : Float;
	var virtualStartTime(default,null) : Float;
	function new(init_type : String, init_virtualStartTime : Float, init_virtualDuration : Float, init_firstPeriodIndex : Int, init_lastPeriodIndex : Int, init_complete : Bool) : Void;
}
