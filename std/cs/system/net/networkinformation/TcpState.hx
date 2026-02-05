package cs.system.net.networkinformation;

/** Specifies the states of a Transmission Control Protocol (TCP) connection. */
@:native("System.Net.NetworkInformation.TcpState")
extern enum abstract TcpState(Int) {
	var Closed = 1;
	var CloseWait = 8;
	var Closing = 9;
	var DeleteTcb = 12;
	var Established = 5;
	var FinWait1 = 6;
	var FinWait2 = 7;
	var LastAck = 10;
	var Listen = 2;
	var SynReceived = 4;
	var SynSent = 3;
	var TimeWait = 11;
	var Unknown = 0;
}
