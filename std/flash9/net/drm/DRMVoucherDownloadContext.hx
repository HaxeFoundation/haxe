package flash.net.drm;

extern class DRMVoucherDownloadContext extends DRMManagerSession {
	var voucher(default,null) : DRMVoucher;
	function new() : Void;
	function download(inMetadata : DRMContentData, previewVoucher : Bool = false) : Void;
}
