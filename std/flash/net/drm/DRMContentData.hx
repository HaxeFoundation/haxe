package flash.net.drm;

extern class DRMContentData {
	var authenticationMethod(default,never) : String;
	var domain(default,never) : String;
	var licenseID(default,never) : String;
	var serverURL(default,never) : String;
	function new(?rawData : flash.utils.ByteArray) : Void;
	function getVoucherAccessInfo() : flash.Vector<VoucherAccessInfo>;
}
