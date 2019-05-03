package flash.net.drm;

extern class DRMVoucherStoreContext extends DRMManagerSession {
	var voucher(get,never) : DRMVoucher;
	function new() : Void;
	function getVoucherFromStore(inMetadata : DRMContentData) : Void;
	private function get_voucher() : DRMVoucher;
}
