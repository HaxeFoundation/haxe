package flash.net.drm;

extern class DRMVoucher {
	var licenseID(get,never) : String;
	var offlineLeaseEndDate(get,never) : Date;
	var offlineLeaseStartDate(get,never) : Date;
	var playbackTimeWindow(get,never) : DRMPlaybackTimeWindow;
	var policies(get,never) : Dynamic;
	var policyID(get,never) : String;
	var serverURL(get,never) : String;
	var voucherEndDate(get,never) : Date;
	var voucherStartDate(get,never) : Date;
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
