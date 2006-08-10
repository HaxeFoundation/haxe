package flash.display;

extern class FrameLabel {
	function new(name : String, frame : Int) : Void;
	var frame(default,null) : Int;
	var name(default,null) : String;
	private var _frame : Int;
	private var _name : String;
}
