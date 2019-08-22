package flash.net.drm;

extern class DRMPlaybackTimeWindow {
	@:flash.property var endDate(get,never) : Date;
	@:flash.property var period(get,never) : UInt;
	@:flash.property var startDate(get,never) : Date;
	function new() : Void;
	private function get_endDate() : Date;
	private function get_period() : UInt;
	private function get_startDate() : Date;
}
