package flash.text.engine;

extern class TabStop {
	function new(?alignment : String, ?position : Float, ?decimalAlignmentToken : String) : Void;
	var alignment : String;
	var decimalAlignmentToken : String;
	var position : Float;
}
