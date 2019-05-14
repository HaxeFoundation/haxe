package flash.events;

@:require(flash10_1) extern class DRMErrorEvent extends ErrorEvent {
	@:flash.property var contentData(get,set) : flash.net.drm.DRMContentData;
	@:flash.property var drmUpdateNeeded(get,never) : Bool;
	@:flash.property var subErrorID(get,never) : Int;
	@:flash.property var systemUpdateNeeded(get,never) : Bool;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inErrorDetail : String, inErrorCode : Int = 0, insubErrorID : Int = 0, ?inMetadata : flash.net.drm.DRMContentData, inSystemUpdateNeeded : Bool = false, inDrmUpdateNeeded : Bool = false) : Void;
	private function get_contentData() : flash.net.drm.DRMContentData;
	private function get_drmUpdateNeeded() : Bool;
	private function get_subErrorID() : Int;
	private function get_systemUpdateNeeded() : Bool;
	private function set_contentData(value : flash.net.drm.DRMContentData) : flash.net.drm.DRMContentData;
	static final DRM_ERROR : String;
	static final DRM_LOAD_DEVICEID_ERROR : String;
}
