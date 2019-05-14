package flash.text.engine;

extern final class TabStop {
	@:flash.property var alignment(get,set) : TabAlignment;
	@:flash.property var decimalAlignmentToken(get,set) : String;
	@:flash.property var position(get,set) : Float;
	function new(?alignment : TabAlignment, position : Float = 0, ?decimalAlignmentToken : String) : Void;
	private function get_alignment() : TabAlignment;
	private function get_decimalAlignmentToken() : String;
	private function get_position() : Float;
	private function set_alignment(value : TabAlignment) : TabAlignment;
	private function set_decimalAlignmentToken(value : String) : String;
	private function set_position(value : Float) : Float;
}
