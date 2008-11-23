package flash.text.engine;

extern class TabStop {
	function new(?alignment : flash.text.engine.TabAlignment, ?position : Float, ?decimalAlignmentToken : String) : Void;
	var alignment : flash.text.engine.TabAlignment;
	var decimalAlignmentToken : String;
	var position : Float;
}
