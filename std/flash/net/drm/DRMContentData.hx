package flash.net.drm;

extern class DRMContentData {
	var authenticationMethod(get,never) : String;
	var domain(get,never) : String;
	var licenseID(get,never) : String;
	var serverURL(get,never) : String;
	function new(?rawData : flash.utils.ByteArray) : Void;
	function getVoucherAccessInfo() : flash.Vector<VoucherAccessInfo>;
	private function get_authenticationMethod() : String;
	private function get_domain() : String;
	private function get_licenseID() : String;
	private function get_serverURL() : String;
}
