package flash.net.drm;

extern class DRMVoucher {
	@:flash.property var licenseID(get,never) : String;
	@:flash.property var offlineLeaseEndDate(get,never) : Date;
	@:flash.property var offlineLeaseStartDate(get,never) : Date;
	@:flash.property var playbackTimeWindow(get,never) : DRMPlaybackTimeWindow;
	@:flash.property var policies(get,never) : Dynamic;
	@:flash.property var policyID(get,never) : String;
	@:flash.property var serverURL(get,never) : String;
	@:flash.property var voucherEndDate(get,never) : Date;
	@:flash.property var voucherStartDate(get,never) : Date;
	function new() : Void;
	private function get_licenseID() : String;
	private function get_offlineLeaseEndDate() : Date;
	private function get_offlineLeaseStartDate() : Date;
	private function get_playbackTimeWindow() : DRMPlaybackTimeWindow;
	private function get_policies() : Dynamic;
	private function get_policyID() : String;
	private function get_serverURL() : String;
	private function get_voucherEndDate() : Date;
	private function get_voucherStartDate() : Date;
	function toByteArray() : flash.utils.ByteArray;
}
