package flash.text.engine;

extern final class SpaceJustifier extends TextJustifier {
	var letterSpacing(get,set) : Bool;
	@:require(flash10_1) var maximumSpacing(get,set) : Float;
	@:require(flash10_1) var minimumSpacing(get,set) : Float;
	@:require(flash10_1) var optimumSpacing(get,set) : Float;
	function new(?locale : String, ?lineJustification : LineJustification, letterSpacing : Bool = false) : Void;
	private function get_letterSpacing() : Bool;
	private function get_maximumSpacing() : Float;
	private function get_minimumSpacing() : Float;
	private function get_optimumSpacing() : Float;
	private function set_letterSpacing(value : Bool) : Bool;
	private function set_maximumSpacing(value : Float) : Float;
	private function set_minimumSpacing(value : Float) : Float;
	private function set_optimumSpacing(value : Float) : Float;
}
