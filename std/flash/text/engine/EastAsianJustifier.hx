package flash.text.engine;

extern final class EastAsianJustifier extends TextJustifier {
	var composeTrailingIdeographicSpaces : Bool;
	var justificationStyle : JustificationStyle;
	function new(?locale : String, ?lineJustification : LineJustification, ?justificationStyle : JustificationStyle) : Void;
}
