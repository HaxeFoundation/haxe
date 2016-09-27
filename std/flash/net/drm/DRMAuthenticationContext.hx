package flash.net.drm;

extern class DRMAuthenticationContext extends DRMManagerSession {
	var authenticationToken(default,never) : flash.utils.ByteArray;
	function new() : Void;
	function authenticate(url : String, domain : String, username : String, password : String) : Void;
}
