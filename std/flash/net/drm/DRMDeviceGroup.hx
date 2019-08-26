package flash.net.drm;

extern final class DRMDeviceGroup {
	@:flash.property var authenticationMethod(get,never) : String;
	@:flash.property var domain(get,never) : String;
	@:flash.property var name(get,set) : String;
	@:flash.property var serverURL(get,never) : String;
	function new() : Void;
	private function get_authenticationMethod() : String;
	private function get_domain() : String;
	private function get_name() : String;
	private function get_serverURL() : String;
	private function set_name(value : String) : String;
}
