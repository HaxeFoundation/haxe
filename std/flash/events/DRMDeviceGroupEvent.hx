package flash.events;

extern class DRMDeviceGroupEvent extends Event {
	var deviceGroup : flash.net.drm.DRMDeviceGroup;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?deviceGroup : flash.net.drm.DRMDeviceGroup) : Void;
	static final ADD_TO_DEVICE_GROUP_COMPLETE : String;
	static final REMOVE_FROM_DEVICE_GROUP_COMPLETE : String;
}
