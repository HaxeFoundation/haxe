package flash.text.engine;

extern final class EastAsianJustifier extends TextJustifier {
	var composeTrailingIdeographicSpaces(get,set) : Bool;
	var justificationStyle(get,set) : JustificationStyle;
	function new(?locale : String, ?lineJustification : LineJustification, ?justificationStyle : JustificationStyle) : Void;
	private function get_composeTrailingIdeographicSpaces() : Bool;
	private function get_justificationStyle() : JustificationStyle;
	private function set_composeTrailingIdeographicSpaces(value : Bool) : Bool;
	private function set_justificationStyle(value : JustificationStyle) : JustificationStyle;
}
