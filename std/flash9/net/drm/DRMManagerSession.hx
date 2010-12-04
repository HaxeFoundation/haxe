package flash.net.drm;

extern class DRMManagerSession extends flash.events.EventDispatcher {
	var m_isInSession : Bool;
	var metadata : DRMContentData;
	function new() : Void;
	function checkStatus() : UInt;
	function errorCodeToThrow(errorCode : UInt) : Void;
	function getLastError() : UInt;
	function getLastSubErrorID() : UInt;
	function issueDRMErrorEvent(metadata : DRMContentData, errorID : Int, subErrorID : Int) : Void;
	function issueDRMStatusEvent(inMetadata : DRMContentData, voucher : DRMVoucher) : Dynamic;
	function onSessionComplete() : Void;
	function onSessionError() : Void;
	function setTimerUp() : Void;
	static var STATUS_FAILED : UInt;
	static var STATUS_NOTREADY : UInt;
	static var STATUS_READY : UInt;
	static var STATUS_UNKNOWN : UInt;
}
