package flash.net.drm;

extern final class VoucherAccessInfo {
	var authenticationMethod(get,never) : String;
	var deviceGroup(get,never) : DRMDeviceGroup;
	var displayName(get,never) : String;
	var domain(get,never) : String;
	var policyID(get,never) : String;
	function new() : Void;
	private function get_authenticationMethod() : String;
	private function get_deviceGroup() : DRMDeviceGroup;
	private function get_displayName() : String;
	private function get_domain() : String;
	private function get_policyID() : String;
}
