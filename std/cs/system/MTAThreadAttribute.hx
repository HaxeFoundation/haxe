package cs.system;

/** Indicates that the COM threading model for an application is multithreaded apartment (MTA). */
@:native("System.MTAThreadAttribute")
extern class MTAThreadAttribute extends cs.system.Attribute {
	function new():Void;
}
