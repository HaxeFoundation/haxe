package flash.events;

extern class DRMReturnVoucherCompleteEvent extends Event {
	@:flash.property var licenseID(get,set) : String;
	@:flash.property var numberOfVouchersReturned(get,set) : Int;
	@:flash.property var policyID(get,set) : String;
	@:flash.property var serverURL(get,set) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inServerURL : String, ?inLicenseID : String, ?inPolicyID : String, inNumberOfVouchersReturned : Int = 0) : Void;
	private function get_licenseID() : String;
	private function get_numberOfVouchersReturned() : Int;
	private function get_policyID() : String;
	private function get_serverURL() : String;
	private function set_licenseID(value : String) : String;
	private function set_numberOfVouchersReturned(value : Int) : Int;
	private function set_policyID(value : String) : String;
	private function set_serverURL(value : String) : String;
	static final RETURN_VOUCHER_COMPLETE : String;
}
