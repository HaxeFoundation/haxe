package flash.net.drm;

extern class DRMVoucher {
	var licenseID(default,never) : String;
	var offlineLeaseEndDate(default,never) : Date;
	var offlineLeaseStartDate(default,never) : Date;
	var playbackTimeWindow(default,never) : DRMPlaybackTimeWindow;
	var policies(default,never) : Dynamic;
	var policyID(default,never) : String;
	var serverURL(default,never) : String;
	var voucherEndDate(default,never) : Date;
	var voucherStartDate(default,never) : Date;
	function new() : Void;
	function toByteArray() : flash.utils.ByteArray;
}
