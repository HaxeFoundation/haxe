package flash.net.drm;

extern class DRMManager extends flash.events.EventDispatcher {
	function new() : Void;
	function addToDeviceGroup(deviceGroup : DRMDeviceGroup, forceRefresh : Bool = false) : Void;
	function authenticate(serverURL : String, domain : String, username : String, password : String) : Void;
	function loadPreviewVoucher(contentData : DRMContentData) : Void;
	function loadVoucher(contentData : DRMContentData, setting : String) : Void;
	function removeFromDeviceGroup(deviceGroup : DRMDeviceGroup) : Void;
	function resetDRMVouchers() : Void;
	@:ns("flash.net.drm",internal) function resetDRMVouchersInternal(isAutoReset : Bool) : Void;
	function returnVoucher(inServerURL : String, immediateCommit : Bool, licenseID : String, policyID : String) : Void;
	function setAuthenticationToken(serverUrl : String, domain : String, token : flash.utils.ByteArray) : Void;
	function storeVoucher(voucher : flash.utils.ByteArray) : Void;
	@:flash.property static var isSupported(get,never) : Bool;
	@:flash.property static var networkIdleTimeout(get,set) : Float;
	static function getDRMManager() : DRMManager;
	@:ns("flash.net.drm",internal) static function getDRMManagerInternal() : DRMManager;
	private static function get_isSupported() : Bool;
	private static function get_networkIdleTimeout() : Float;
	private static function set_networkIdleTimeout(value : Float) : Float;
}
