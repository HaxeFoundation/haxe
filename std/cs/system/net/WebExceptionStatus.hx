package cs.system.net;

/** Defines status codes for the  class. */
@:native("System.Net.WebExceptionStatus")
extern enum WebExceptionStatus {
	CacheEntryNotFound;
	ConnectFailure;
	ConnectionClosed;
	KeepAliveFailure;
	MessageLengthLimitExceeded;
	NameResolutionFailure;
	Pending;
	PipelineFailure;
	ProtocolError;
	ProxyNameResolutionFailure;
	ReceiveFailure;
	RequestCanceled;
	RequestProhibitedByCachePolicy;
	RequestProhibitedByProxy;
	SecureChannelFailure;
	SendFailure;
	ServerProtocolViolation;
	Success;
	Timeout;
	TrustFailure;
	UnknownError;
}
