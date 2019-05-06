package flash.net.drm;

extern final class VoucherAccessInfo {
	@:flash.property var authenticationMethod(get,never) : String;
	@:flash.property var deviceGroup(get,never) : DRMDeviceGroup;
	@:flash.property var displayName(get,never) : String;
	@:flash.property var domain(get,never) : String;
	@:flash.property var policyID(get,never) : String;
	function new() : Void;
	private function get_authenticationMethod() : String;
	private function get_deviceGroup() : DRMDeviceGroup;
	private function get_displayName() : String;
	private function get_domain() : String;
	private function get_policyID() : String;
}
