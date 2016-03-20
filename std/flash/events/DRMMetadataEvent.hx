package flash.events;

extern class DRMMetadataEvent extends Event {
	var drmMetadata(default,never) : flash.net.drm.DRMContentData;
	var timestamp(default,never) : Float;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inMetadata : flash.utils.ByteArray, inTimestamp : Float = 0) : Void;
	static var DRM_METADATA(default,never) : String;
}
