package flash.net.drm;

extern final class DRMDeviceGroup {
	var authenticationMethod(get,never) : String;
	var domain(get,never) : String;
	var name(get,set) : String;
	var serverURL(get,never) : String;
	function new() : Void;
	private function get_authenticationMethod() : String;
	private function get_domain() : String;
	private function get_name() : String;
	private function get_serverURL() : String;
	private function set_name(value : String) : String;
}
