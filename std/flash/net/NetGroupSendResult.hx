package flash.net;

extern class NetGroupSendResult {
	function new() : Void;
	static var ERROR(default,never) : String;
	static var NO_ROUTE(default,never) : String;
	static var SENT(default,never) : String;
}
