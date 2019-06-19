package flash.events;

extern class DRMMetadataEvent extends Event {
	@:flash.property var drmMetadata(get,never) : flash.net.drm.DRMContentData;
	@:flash.property var timestamp(get,never) : Float;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inMetadata : flash.utils.ByteArray, inTimestamp : Float = 0) : Void;
	private function get_drmMetadata() : flash.net.drm.DRMContentData;
	private function get_timestamp() : Float;
	static final DRM_METADATA : String;
}
