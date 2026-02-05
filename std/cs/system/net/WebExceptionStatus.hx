package cs.system.net;

/** Defines status codes for the  class. */
@:native("System.Net.WebExceptionStatus")
extern enum abstract WebExceptionStatus(Int) {
	var CacheEntryNotFound = 18;
	var ConnectFailure = 2;
	var ConnectionClosed = 8;
	var KeepAliveFailure = 12;
	var MessageLengthLimitExceeded = 17;
	var NameResolutionFailure = 1;
	var Pending = 13;
	var PipelineFailure = 5;
	var ProtocolError = 7;
	var ProxyNameResolutionFailure = 15;
	var ReceiveFailure = 3;
	var RequestCanceled = 6;
	var RequestProhibitedByCachePolicy = 19;
	var RequestProhibitedByProxy = 20;
	var SecureChannelFailure = 10;
	var SendFailure = 4;
	var ServerProtocolViolation = 11;
	var Success = 0;
	var Timeout = 14;
	var TrustFailure = 9;
	var UnknownError = 16;
}
