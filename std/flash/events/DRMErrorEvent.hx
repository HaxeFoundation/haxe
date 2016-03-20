package flash.events;

@:require(flash10_1) extern class DRMErrorEvent extends ErrorEvent {
	var contentData : flash.net.drm.DRMContentData;
	var drmUpdateNeeded(default,never) : Bool;
	var subErrorID(default,never) : Int;
	var systemUpdateNeeded(default,never) : Bool;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inErrorDetail : String, inErrorCode : Int = 0, insubErrorID : Int = 0, ?inMetadata : flash.net.drm.DRMContentData, inSystemUpdateNeeded : Bool = false, inDrmUpdateNeeded : Bool = false) : Void;
	static var DRM_ERROR(default,never) : String;
	static var DRM_LOAD_DEVICEID_ERROR(default,never) : String;
}
