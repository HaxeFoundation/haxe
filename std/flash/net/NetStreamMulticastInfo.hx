package flash.net;

@:final @:require(flash10_1) extern class NetStreamMulticastInfo {
	var bytesPushedFromPeers(default,never) : Float;
	var bytesPushedToPeers(default,never) : Float;
	var bytesReceivedFromIPMulticast(default,never) : Float;
	var bytesReceivedFromServer(default,never) : Float;
	var bytesRequestedByPeers(default,never) : Float;
	var bytesRequestedFromPeers(default,never) : Float;
	var fragmentsPushedFromPeers(default,never) : Float;
	var fragmentsPushedToPeers(default,never) : Float;
	var fragmentsReceivedFromIPMulticast(default,never) : Float;
	var fragmentsReceivedFromServer(default,never) : Float;
	var fragmentsRequestedByPeers(default,never) : Float;
	var fragmentsRequestedFromPeers(default,never) : Float;
	var receiveControlBytesPerSecond(default,never) : Float;
	var receiveDataBytesPerSecond(default,never) : Float;
	var receiveDataBytesPerSecondFromIPMulticast(default,never) : Float;
	var receiveDataBytesPerSecondFromServer(default,never) : Float;
	var sendControlBytesPerSecond(default,never) : Float;
	var sendControlBytesPerSecondToServer(default,never) : Float;
	var sendDataBytesPerSecond(default,never) : Float;
	function new(sendDataBytesPerSecond : Float, sendControlBytesPerSecond : Float, receiveDataBytesPerSecond : Float, receiveControlBytesPerSecond : Float, bytesPushedToPeers : Float, fragmentsPushedToPeers : Float, bytesRequestedByPeers : Float, fragmentsRequestedByPeers : Float, bytesPushedFromPeers : Float, fragmentsPushedFromPeers : Float, bytesRequestedFromPeers : Float, fragmentsRequestedFromPeers : Float, sendControlBytesPerSecondToServer : Float, receiveDataBytesPerSecondFromServer : Float, bytesReceivedFromServer : Float, fragmentsReceivedFromServer : Float, receiveDataBytesPerSecondFromIPMulticast : Float, bytesReceivedFromIPMulticast : Float, fragmentsReceivedFromIPMulticast : Float) : Void;
	function toString() : String;
}
