package flash.net.drm;

extern final class VoucherAccessInfo {
	var authenticationMethod(default,never) : String;
	var deviceGroup(default,never) : DRMDeviceGroup;
	var displayName(default,never) : String;
	var domain(default,never) : String;
	var policyID(default,never) : String;
	function new() : Void;
}
