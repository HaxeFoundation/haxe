package flash.net.drm;

extern class DRMVoucher {
	var licenseID(default,null) : String;
	var offlineLeaseEndDate(default,null) : Date;
	var offlineLeaseStartDate(default,null) : Date;
	var playbackTimeWindow(default,null) : DRMPlaybackTimeWindow;
	var policies(default,null) : Dynamic;
	var policyID(default,null) : String;
	var serverURL(default,null) : String;
	var voucherEndDate(default,null) : Date;
	var voucherStartDate(default,null) : Date;
	function new() : Void;
	function toByteArray() : flash.utils.ByteArray;
}
