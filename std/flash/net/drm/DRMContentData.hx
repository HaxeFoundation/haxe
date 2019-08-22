package flash.net.drm;

extern class DRMContentData {
	@:flash.property var authenticationMethod(get,never) : String;
	@:flash.property var domain(get,never) : String;
	@:flash.property var licenseID(get,never) : String;
	@:flash.property var serverURL(get,never) : String;
	function new(?rawData : flash.utils.ByteArray) : Void;
	function getVoucherAccessInfo() : flash.Vector<VoucherAccessInfo>;
	private function get_authenticationMethod() : String;
	private function get_domain() : String;
	private function get_licenseID() : String;
	private function get_serverURL() : String;
}
