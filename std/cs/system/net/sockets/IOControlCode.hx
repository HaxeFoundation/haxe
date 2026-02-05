package cs.system.net.sockets;

/** Specifies the IO control codes supported by the  method. */
@:native("System.Net.Sockets.IOControlCode")
extern enum IOControlCode {
	AbsorbRouterAlert;
	AddMulticastGroupOnInterface;
	AddressListChange;
	AddressListQuery;
	AddressListSort;
	AssociateHandle;
	AsyncIO;
	BindToInterface;
	DataToRead;
	DeleteMulticastGroupFromInterface;
	EnableCircularQueuing;
	Flush;
	GetBroadcastAddress;
	GetExtensionFunctionPointer;
	GetGroupQos;
	GetQos;
	KeepAliveValues;
	LimitBroadcasts;
	MulticastInterface;
	MulticastScope;
	MultipointLoopback;
	NamespaceChange;
	NonBlockingIO;
	OobDataRead;
	QueryTargetPnpHandle;
	ReceiveAll;
	ReceiveAllIgmpMulticast;
	ReceiveAllMulticast;
	RoutingInterfaceChange;
	RoutingInterfaceQuery;
	SetGroupQos;
	SetQos;
	TranslateHandle;
	UnicastInterface;
}
