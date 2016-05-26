package flash.net;

@:require(flash10_1) extern class NetGroup extends flash.events.EventDispatcher {
	var estimatedMemberCount(default,never) : Float;
	var info(default,never) : NetGroupInfo;
	var localCoverageFrom(default,never) : String;
	var localCoverageTo(default,never) : String;
	var neighborCount(default,never) : Float;
	var receiveMode : String;
	var replicationStrategy : String;
	function new(connection : NetConnection, groupspec : String) : Void;
	function addHaveObjects(startIndex : Float, endIndex : Float) : Void;
	function addMemberHint(peerID : String) : Bool;
	function addNeighbor(peerID : String) : Bool;
	function addWantObjects(startIndex : Float, endIndex : Float) : Void;
	function close() : Void;
	function convertPeerIDToGroupAddress(peerID : String) : String;
	function denyRequestedObject(requestID : Int) : Void;
	function post(message : flash.utils.Object) : String;
	function removeHaveObjects(startIndex : Float, endIndex : Float) : Void;
	function removeWantObjects(startIndex : Float, endIndex : Float) : Void;
	function sendToAllNeighbors(message : flash.utils.Object) : String;
	function sendToNearest(message : flash.utils.Object, groupAddress : String) : String;
	function sendToNeighbor(message : flash.utils.Object, sendMode : String) : String;
	function writeRequestedObject(requestID : Int, object : flash.utils.Object) : Void;
}
