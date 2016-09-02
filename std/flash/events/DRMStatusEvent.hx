package flash.events;

@:require(flash10_1) extern class DRMStatusEvent extends Event {
	var contentData : flash.net.drm.DRMContentData;
	var isLocal : Bool;
	var voucher : flash.net.drm.DRMVoucher;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inMetadata : flash.net.drm.DRMContentData, ?inVoucher : flash.net.drm.DRMVoucher, inLocal : Bool = false) : Void;
	static var DRM_STATUS(default,never) : String;
}
