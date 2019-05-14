package flash.events;

extern class DRMDeviceGroupErrorEvent extends ErrorEvent {
	@:flash.property var deviceGroup(get,set) : flash.net.drm.DRMDeviceGroup;
	@:flash.property var drmUpdateNeeded(get,never) : Bool;
	@:flash.property var subErrorID(get,set) : Int;
	@:flash.property var systemUpdateNeeded(get,never) : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?errorDetail : String, errorCode : Int = 0, subErrorID : Int = 0, ?deviceGroup : flash.net.drm.DRMDeviceGroup, systemUpdateNeeded : Bool = false, drmUpdateNeeded : Bool = false) : Void;
	private function get_deviceGroup() : flash.net.drm.DRMDeviceGroup;
	private function get_drmUpdateNeeded() : Bool;
	private function get_subErrorID() : Int;
	private function get_systemUpdateNeeded() : Bool;
	private function set_deviceGroup(value : flash.net.drm.DRMDeviceGroup) : flash.net.drm.DRMDeviceGroup;
	private function set_subErrorID(value : Int) : Int;
	static final ADD_TO_DEVICE_GROUP_ERROR : String;
	static final REMOVE_FROM_DEVICE_GROUP_ERROR : String;
}
