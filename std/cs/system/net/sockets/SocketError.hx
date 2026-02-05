package cs.system.net.sockets;

/** Defines error codes for the  class. */
@:native("System.Net.Sockets.SocketError")
extern enum abstract SocketError(Int) {
	var AccessDenied = 10013;
	var AddressAlreadyInUse = 10048;
	var AddressFamilyNotSupported = 10047;
	var AddressNotAvailable = 10049;
	var AlreadyInProgress = 10037;
	var ConnectionAborted = 10053;
	var ConnectionRefused = 10061;
	var ConnectionReset = 10054;
	var DestinationAddressRequired = 10039;
	var Disconnecting = 10101;
	var Fault = 10014;
	var HostDown = 10064;
	var HostNotFound = 11001;
	var HostUnreachable = 10065;
	var InProgress = 10036;
	var Interrupted = 10004;
	var InvalidArgument = 10022;
	var IOPending = 997;
	var IsConnected = 10056;
	var MessageSize = 10040;
	var NetworkDown = 10050;
	var NetworkReset = 10052;
	var NetworkUnreachable = 10051;
	var NoBufferSpaceAvailable = 10055;
	var NoData = 11004;
	var NoRecovery = 11003;
	var NotConnected = 10057;
	var NotInitialized = 10093;
	var NotSocket = 10038;
	var OperationAborted = 995;
	var OperationNotSupported = 10045;
	var ProcessLimit = 10067;
	var ProtocolFamilyNotSupported = 10046;
	var ProtocolNotSupported = 10043;
	var ProtocolOption = 10042;
	var ProtocolType = 10041;
	var Shutdown = 10058;
	var SocketError = -1;
	var SocketNotSupported = 10044;
	var Success = 0;
	var SystemNotReady = 10091;
	var TimedOut = 10060;
	var TooManyOpenSockets = 10024;
	var TryAgain = 11002;
	var TypeNotFound = 10109;
	var VersionNotSupported = 10092;
	var WouldBlock = 10035;
}
