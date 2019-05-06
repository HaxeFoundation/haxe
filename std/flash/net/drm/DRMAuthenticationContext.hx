package flash.net.drm;

extern class DRMAuthenticationContext extends DRMManagerSession {
	@:flash.property var authenticationToken(get,never) : flash.utils.ByteArray;
	function new() : Void;
	function authenticate(url : String, domain : String, username : String, password : String) : Void;
	private function get_authenticationToken() : flash.utils.ByteArray;
}
