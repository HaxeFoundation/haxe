package flash.text.engine;

extern final class TabStop {
	var alignment : TabAlignment;
	var decimalAlignmentToken : String;
	var position : Float;
	function new(?alignment : TabAlignment, position : Float = 0, ?decimalAlignmentToken : String) : Void;
}
