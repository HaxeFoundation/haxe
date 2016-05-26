package flash.events;

extern class DRMReturnVoucherCompleteEvent extends Event {
	var licenseID : String;
	var numberOfVouchersReturned : Int;
	var policyID : String;
	var serverURL : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inServerURL : String, ?inLicenseID : String, ?inPolicyID : String, inNumberOfVouchersReturned : Int = 0) : Void;
	static var RETURN_VOUCHER_COMPLETE(default,never) : String;
}
