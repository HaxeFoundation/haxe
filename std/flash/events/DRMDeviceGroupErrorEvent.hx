package flash.events;

extern class DRMDeviceGroupErrorEvent extends ErrorEvent {
	var deviceGroup : flash.net.drm.DRMDeviceGroup;
	var drmUpdateNeeded(default,never) : Bool;
	var subErrorID : Int;
	var systemUpdateNeeded(default,never) : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?errorDetail : String, errorCode : Int = 0, subErrorID : Int = 0, ?deviceGroup : flash.net.drm.DRMDeviceGroup, systemUpdateNeeded : Bool = false, drmUpdateNeeded : Bool = false) : Void;
	static var ADD_TO_DEVICE_GROUP_ERROR(default,never) : String;
	static var REMOVE_FROM_DEVICE_GROUP_ERROR(default,never) : String;
}
