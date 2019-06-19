package flash.events;

@:require(flash10_1) extern class DRMStatusEvent extends Event {
	@:flash.property var contentData(get,set) : flash.net.drm.DRMContentData;
	@:flash.property var isLocal(get,set) : Bool;
	@:flash.property var voucher(get,set) : flash.net.drm.DRMVoucher;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inMetadata : flash.net.drm.DRMContentData, ?inVoucher : flash.net.drm.DRMVoucher, inLocal : Bool = false) : Void;
	private function get_contentData() : flash.net.drm.DRMContentData;
	private function get_isLocal() : Bool;
	private function get_voucher() : flash.net.drm.DRMVoucher;
	private function set_contentData(value : flash.net.drm.DRMContentData) : flash.net.drm.DRMContentData;
	private function set_isLocal(value : Bool) : Bool;
	private function set_voucher(value : flash.net.drm.DRMVoucher) : flash.net.drm.DRMVoucher;
	static final DRM_STATUS : String;
}
