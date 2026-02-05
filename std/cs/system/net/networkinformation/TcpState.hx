package cs.system.net.networkinformation;

/** Specifies the states of a Transmission Control Protocol (TCP) connection. */
@:native("System.Net.NetworkInformation.TcpState")
extern enum TcpState {
	Closed;
	CloseWait;
	Closing;
	DeleteTcb;
	Established;
	FinWait1;
	FinWait2;
	LastAck;
	Listen;
	SynReceived;
	SynSent;
	TimeWait;
	Unknown;
}
