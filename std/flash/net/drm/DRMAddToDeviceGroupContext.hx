package flash.net.drm;

extern class DRMAddToDeviceGroupContext extends DRMManagerSession {
	function new() : Void;
	function addToDeviceGroup(deviceGroup : DRMDeviceGroup, forceRefresh : Bool) : Void;
}
