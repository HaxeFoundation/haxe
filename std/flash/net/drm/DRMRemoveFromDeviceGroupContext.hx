package flash.net.drm;

extern class DRMRemoveFromDeviceGroupContext extends DRMManagerSession {
	function new() : Void;
	function removeFromDeviceGroup(deviceGroup : DRMDeviceGroup) : Void;
}
