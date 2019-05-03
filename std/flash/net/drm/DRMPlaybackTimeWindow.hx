package flash.net.drm;

extern class DRMPlaybackTimeWindow {
	var endDate(get,never) : Date;
	var period(get,never) : UInt;
	var startDate(get,never) : Date;
	function new() : Void;
	private function get_endDate() : Date;
	private function get_period() : UInt;
	private function get_startDate() : Date;
}
