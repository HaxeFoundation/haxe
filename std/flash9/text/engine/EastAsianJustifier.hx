package flash.text.engine;

@:final extern class EastAsianJustifier extends TextJustifier {
	var justificationStyle : JustificationStyle;
	function new(?locale : String, ?lineJustification : LineJustification, ?justificationStyle : JustificationStyle) : Void;
}
