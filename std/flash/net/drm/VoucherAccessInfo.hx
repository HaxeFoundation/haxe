package flash.net.drm;

@:final extern class VoucherAccessInfo {
	var authenticationMethod(default,null) : String;
	var deviceGroup(default,null) : DRMDeviceGroup;
	var displayName(default,null) : String;
	var domain(default,null) : String;
	var policyID(default,null) : String;
	function new() : Void;
}
