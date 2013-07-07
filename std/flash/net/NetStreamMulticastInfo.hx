package flash.net;

@:final @:require(flash10_1) extern class NetStreamMulticastInfo {
	var bytesPushedFromPeers(default,null) : Float;
	var bytesPushedToPeers(default,null) : Float;
	var bytesReceivedFromIPMulticast(default,null) : Float;
	var bytesReceivedFromServer(default,null) : Float;
	var bytesRequestedByPeers(default,null) : Float;
	var bytesRequestedFromPeers(default,null) : Float;
	var fragmentsPushedFromPeers(default,null) : Float;
	var fragmentsPushedToPeers(default,null) : Float;
	var fragmentsReceivedFromIPMulticast(default,null) : Float;
	var fragmentsReceivedFromServer(default,null) : Float;
	var fragmentsRequestedByPeers(default,null) : Float;
	var fragmentsRequestedFromPeers(default,null) : Float;
	var receiveControlBytesPerSecond(default,null) : Float;
	var receiveDataBytesPerSecond(default,null) : Float;
	var receiveDataBytesPerSecondFromIPMulticast(default,null) : Float;
	var receiveDataBytesPerSecondFromServer(default,null) : Float;
	var sendControlBytesPerSecond(default,null) : Float;
	var sendControlBytesPerSecondToServer(default,null) : Float;
	var sendDataBytesPerSecond(default,null) : Float;
	function new(sendDataBytesPerSecond : Float, sendControlBytesPerSecond : Float, receiveDataBytesPerSecond : Float, receiveControlBytesPerSecond : Float, bytesPushedToPeers : Float, fragmentsPushedToPeers : Float, bytesRequestedByPeers : Float, fragmentsRequestedByPeers : Float, bytesPushedFromPeers : Float, fragmentsPushedFromPeers : Float, bytesRequestedFromPeers : Float, fragmentsRequestedFromPeers : Float, sendControlBytesPerSecondToServer : Float, receiveDataBytesPerSecondFromServer : Float, bytesReceivedFromServer : Float, fragmentsReceivedFromServer : Float, receiveDataBytesPerSecondFromIPMulticast : Float, bytesReceivedFromIPMulticast : Float, fragmentsReceivedFromIPMulticast : Float) : Void;
	function toString() : String;
}
