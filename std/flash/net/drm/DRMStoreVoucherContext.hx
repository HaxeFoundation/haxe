package flash.net.drm;

extern class DRMStoreVoucherContext extends DRMManagerSession {
	function new(voucher : flash.utils.ByteArray) : Void;
	function doStoreVoucher() : Void;
}
