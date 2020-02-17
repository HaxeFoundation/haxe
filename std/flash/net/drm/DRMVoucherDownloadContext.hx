package flash.net.drm;

extern class DRMVoucherDownloadContext extends DRMManagerSession {
	@:flash.property var voucher(get,never) : DRMVoucher;
	function new() : Void;
	function download(inMetadata : DRMContentData, previewVoucher : Bool = false) : Void;
	private function get_voucher() : DRMVoucher;
}
