package flash.text.engine;

extern class EastAsianJustifier extends flash.text.engine.TextJustifier {
	function new(?locale : String, ?lineJustification : flash.text.engine.LineJustification, ?justificationStyle : flash.text.engine.JustificationStyle) : Void;
	var justificationStyle : flash.text.engine.JustificationStyle;
}
