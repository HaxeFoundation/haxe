package flash.net;

@:require(flash10_1) extern class NetGroup extends flash.events.EventDispatcher {
	@:flash.property var estimatedMemberCount(get,never) : Float;
	@:flash.property var info(get,never) : NetGroupInfo;
	@:flash.property var localCoverageFrom(get,never) : String;
	@:flash.property var localCoverageTo(get,never) : String;
	@:flash.property var neighborCount(get,never) : Float;
	@:flash.property var receiveMode(get,set) : String;
	@:flash.property var replicationStrategy(get,set) : String;
	function new(connection : NetConnection, groupspec : String) : Void;
	function addHaveObjects(startIndex : Float, endIndex : Float) : Void;
	function addMemberHint(peerID : String) : Bool;
	function addNeighbor(peerID : String) : Bool;
	function addWantObjects(startIndex : Float, endIndex : Float) : Void;
	function close() : Void;
	function convertPeerIDToGroupAddress(peerID : String) : String;
	function denyRequestedObject(requestID : Int) : Void;
	private function get_estimatedMemberCount() : Float;
	private function get_info() : NetGroupInfo;
	private function get_localCoverageFrom() : String;
	private function get_localCoverageTo() : String;
	private function get_neighborCount() : Float;
	private function get_receiveMode() : String;
	private function get_replicationStrategy() : String;
	function post(message : flash.utils.Object) : String;
	function removeHaveObjects(startIndex : Float, endIndex : Float) : Void;
	function removeWantObjects(startIndex : Float, endIndex : Float) : Void;
	function sendToAllNeighbors(message : flash.utils.Object) : String;
	function sendToNearest(message : flash.utils.Object, groupAddress : String) : String;
	function sendToNeighbor(message : flash.utils.Object, sendMode : String) : String;
	private function set_receiveMode(value : String) : String;
	private function set_replicationStrategy(value : String) : String;
	function writeRequestedObject(requestID : Int, object : flash.utils.Object) : Void;
}
