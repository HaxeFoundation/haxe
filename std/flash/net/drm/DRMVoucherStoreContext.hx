package flash.net.drm;

extern class DRMVoucherStoreContext extends DRMManagerSession {
	var voucher(default,null) : DRMVoucher;
	function new() : Void;
	function getVoucherFromStore(inMetadata : DRMContentData) : Void;
}
