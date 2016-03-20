package flash.display;

@:final extern class FrameLabel extends flash.events.EventDispatcher {
	var frame(default,never) : Int;
	var name(default,never) : String;
	function new(name : String, frame : Int) : Void;
}
