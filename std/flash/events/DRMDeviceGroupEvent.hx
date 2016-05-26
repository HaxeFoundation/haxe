package flash.events;

extern class DRMDeviceGroupEvent extends Event {
	var deviceGroup : flash.net.drm.DRMDeviceGroup;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?deviceGroup : flash.net.drm.DRMDeviceGroup) : Void;
	static var ADD_TO_DEVICE_GROUP_COMPLETE(default,never) : String;
	static var REMOVE_FROM_DEVICE_GROUP_COMPLETE(default,never) : String;
}
