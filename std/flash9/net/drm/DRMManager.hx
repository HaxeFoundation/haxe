package flash.net.drm;

extern class DRMManager extends flash.events.EventDispatcher {
	function new() : Void;
	function authenticate(serverURL : String, domain : String, username : String, password : String) : Void;
	function loadPreviewVoucher(contentData : DRMContentData) : Void;
	function loadVoucher(contentData : DRMContentData, setting : String) : Void;
	function setAuthenticationToken(serverUrl : String, domain : String, token : flash.utils.ByteArray) : Void;
	static var isSupported(default,null) : Bool;
	static function getDRMManager() : DRMManager;
}
