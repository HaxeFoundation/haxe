package flash.events;

@:require(flash10_1) extern class DRMErrorEvent extends ErrorEvent {
	var contentData : flash.net.drm.DRMContentData;
	var drmUpdateNeeded(default,null) : Bool;
	var subErrorID(default,null) : Int;
	var systemUpdateNeeded(default,null) : Bool;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inErrorDetail : String, inErrorCode : Int = 0, insubErrorID : Int = 0, ?inMetadata : flash.net.drm.DRMContentData, inSystemUpdateNeeded : Bool = false, inDrmUpdateNeeded : Bool = false) : Void;
	static var DRM_ERROR : String;
}
