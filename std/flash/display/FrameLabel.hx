package flash.display;

@:final extern class FrameLabel {
	var frame(default,null) : Int;
	var name(default,null) : String;
	function new(name : String, frame : Int) : Void;
}
