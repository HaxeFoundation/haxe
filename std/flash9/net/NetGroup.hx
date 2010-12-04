package flash.net;

@:require(flash10_1) extern class NetGroup extends flash.events.EventDispatcher {
	var estimatedMemberCount(default,null) : Float;
	var info(default,null) : NetGroupInfo;
	var localCoverageFrom(default,null) : String;
	var localCoverageTo(default,null) : String;
	var neighborCount(default,null) : Float;
	var receiveMode : NetGroupReceiveMode;
	var replicationStrategy : NetGroupReplicationStrategy;
	function new(connection : NetConnection, groupspec : String) : Void;
	function addHaveObjects(startIndex : Float, endIndex : Float) : Void;
	function addMemberHint(peerID : String) : Bool;
	function addNeighbor(peerID : String) : Bool;
	function addWantObjects(startIndex : Float, endIndex : Float) : Void;
	function close() : Void;
	function convertPeerIDToGroupAddress(peerID : String) : String;
	function denyRequestedObject(requestID : Int) : Void;
	function post(message : Dynamic) : String;
	function removeHaveObjects(startIndex : Float, endIndex : Float) : Void;
	function removeWantObjects(startIndex : Float, endIndex : Float) : Void;
	function sendToAllNeighbors(message : Dynamic) : NetGroupSendResult;
	function sendToNearest(message : Dynamic, groupAddress : String) : NetGroupSendResult;
	function sendToNeighbor(message : Dynamic, sendMode : NetGroupSendMode) : NetGroupSendResult;
	function writeRequestedObject(requestID : Int, object : Dynamic) : Void;
}
