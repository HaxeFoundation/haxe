package flash.net;

@:require(flash10_1) extern class GroupSpecifier {
	var ipMulticastMemberUpdatesEnabled : Bool;
	var minGroupspecVersion : Int;
	var multicastEnabled : Bool;
	var objectReplicationEnabled : Bool;
	var peerToPeerDisabled : Bool;
	var postingEnabled : Bool;
	var routingEnabled : Bool;
	var serverChannelEnabled : Bool;
	function new(name : String) : Void;
	function addBootstrapPeer(peerID : String) : Void;
	function addIPMulticastAddress(address : String, ?port : Dynamic, ?source : String) : Void;
	function authorizations() : String;
	function groupspecWithAuthorizations() : String;
	function groupspecWithoutAuthorizations() : String;
	function makeUnique() : Void;
	function setPostingPassword(?password : String, ?salt : String) : Void;
	function setPublishPassword(?password : String, ?salt : String) : Void;
	function toString() : String;
	static var maxSupportedGroupspecVersion(default,never) : Int;
	static function encodeBootstrapPeerIDSpec(peerID : String) : String;
	static function encodeIPMulticastAddressSpec(address : String, ?port : Dynamic, ?source : String) : String;
	static function encodePostingAuthorization(password : String) : String;
	static function encodePublishAuthorization(password : String) : String;
}
