package flash.net.drm;

extern class DRMVoucherDownloadContext extends DRMManagerSession {
	var voucher(default,never) : DRMVoucher;
	function new() : Void;
	function download(inMetadata : DRMContentData, previewVoucher : Bool = false) : Void;
}
