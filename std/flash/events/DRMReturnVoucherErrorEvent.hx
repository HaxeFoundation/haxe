package flash.events;

extern class DRMReturnVoucherErrorEvent extends ErrorEvent {
	var licenseID : String;
	var policyID : String;
	var serverURL : String;
	var subErrorID : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inDetail : String, inErrorID : Int = 0, inSubErrorID : Int = 0, ?inServerURL : String, ?inLicenseID : String, ?inPolicyID : String) : Void;
	static var RETURN_VOUCHER_ERROR(default,never) : String;
}
