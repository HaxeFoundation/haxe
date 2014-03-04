package flash.net.drm;

extern class DRMReturnVoucherContext extends DRMManagerSession {
	function new() : Void;
	function returnVoucher(inServerURL : String, immediateCommit : Bool, licenseID : String, policyID : String) : Void;
}
