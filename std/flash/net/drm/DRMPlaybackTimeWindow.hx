package flash.net.drm;

extern class DRMPlaybackTimeWindow {
	var endDate(default,never) : Date;
	var period(default,never) : UInt;
	var startDate(default,never) : Date;
	function new() : Void;
}
