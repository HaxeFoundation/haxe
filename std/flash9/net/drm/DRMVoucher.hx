package flash.net.drm;

extern class DRMVoucher {
	var offlineLeaseEndDate(default,null) : Date;
	var offlineLeaseStartDate(default,null) : Date;
	var playbackTimeWindow(default,null) : DRMPlaybackTimeWindow;
	var policies(default,null) : Dynamic;
	var voucherEndDate(default,null) : Date;
	var voucherStartDate(default,null) : Date;
	function new() : Void;
}
