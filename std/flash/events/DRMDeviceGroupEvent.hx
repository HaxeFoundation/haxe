package flash.events;

extern class DRMDeviceGroupEvent extends Event {
	@:flash.property var deviceGroup(get,set) : flash.net.drm.DRMDeviceGroup;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?deviceGroup : flash.net.drm.DRMDeviceGroup) : Void;
	private function get_deviceGroup() : flash.net.drm.DRMDeviceGroup;
	private function set_deviceGroup(value : flash.net.drm.DRMDeviceGroup) : flash.net.drm.DRMDeviceGroup;
	static final ADD_TO_DEVICE_GROUP_COMPLETE : String;
	static final REMOVE_FROM_DEVICE_GROUP_COMPLETE : String;
}
