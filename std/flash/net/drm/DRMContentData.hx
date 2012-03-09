package flash.net.drm;

extern class DRMContentData {
	var authenticationMethod(default,null) : String;
	var domain(default,null) : String;
	var licenseID(default,null) : String;
	var serverURL(default,null) : String;
	function new(?rawData : flash.utils.ByteArray) : Void;
	function getVoucherAccessInfo() : flash.Vector<VoucherAccessInfo>;
}
