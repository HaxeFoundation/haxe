package cs.system;

/** Indicates that the COM threading model for an application is single-threaded apartment (STA). */
@:native("System.STAThreadAttribute")
extern class STAThreadAttribute extends cs.system.Attribute {
	function new():Void;
}
