package cs.system.net.sockets;

/** Specifies the IO control codes supported by the  method. */
@:native("System.Net.Sockets.IOControlCode")
extern enum abstract IOControlCode(Int) {
	var AbsorbRouterAlert = -1744830459;
	var AddMulticastGroupOnInterface = -1744830454;
	var AddressListChange = 671088663;
	var AddressListQuery = 1207959574;
	var AddressListSort = -939524071;
	var AssociateHandle = -2013265919;
	var AsyncIO = -2147195267;
	var BindToInterface = -1744830456;
	var DataToRead = 1074030207;
	var DeleteMulticastGroupFromInterface = -1744830453;
	var EnableCircularQueuing = 671088642;
	var Flush = 671088644;
	var GetBroadcastAddress = 1207959557;
	var GetExtensionFunctionPointer = -939524090;
	var GetGroupQos = -939524088;
	var GetQos = -939524089;
	var KeepAliveValues = -1744830460;
	var LimitBroadcasts = -1744830457;
	var MulticastInterface = -1744830455;
	var MulticastScope = -2013265910;
	var MultipointLoopback = -2013265911;
	var NamespaceChange = -2013265895;
	var NonBlockingIO = -2147195266;
	var OobDataRead = 1074033415;
	var QueryTargetPnpHandle = 1207959576;
	var ReceiveAll = -1744830463;
	var ReceiveAllIgmpMulticast = -1744830461;
	var ReceiveAllMulticast = -1744830462;
	var RoutingInterfaceChange = -2013265899;
	var RoutingInterfaceQuery = -939524076;
	var SetGroupQos = -2013265908;
	var SetQos = -2013265909;
	var TranslateHandle = -939524083;
	var UnicastInterface = -1744830458;
}
