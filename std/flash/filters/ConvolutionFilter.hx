package flash.filters;

extern class ConvolutionFilter extends BitmapFilter {
	@:flash.property var alpha(get,set) : Float;
	@:flash.property var bias(get,set) : Float;
	@:flash.property var clamp(get,set) : Bool;
	@:flash.property var color(get,set) : UInt;
	@:flash.property var divisor(get,set) : Float;
	@:flash.property var matrix(get,set) : Array<Dynamic>;
	@:flash.property var matrixX(get,set) : Float;
	@:flash.property var matrixY(get,set) : Float;
	@:flash.property var preserveAlpha(get,set) : Bool;
	function new(matrixX : Float = 0, matrixY : Float = 0, ?matrix : Array<Dynamic>, divisor : Float = 1, bias : Float = 0, preserveAlpha : Bool = true, clamp : Bool = true, color : UInt = 0, alpha : Float = 0) : Void;
	private function get_alpha() : Float;
	private function get_bias() : Float;
	private function get_clamp() : Bool;
	private function get_color() : UInt;
	private function get_divisor() : Float;
	private function get_matrix() : Array<Dynamic>;
	private function get_matrixX() : Float;
	private function get_matrixY() : Float;
	private function get_preserveAlpha() : Bool;
	private function set_alpha(value : Float) : Float;
	private function set_bias(value : Float) : Float;
	private function set_clamp(value : Bool) : Bool;
	private function set_color(value : UInt) : UInt;
	private function set_divisor(value : Float) : Float;
	private function set_matrix(value : Array<Dynamic>) : Array<Dynamic>;
	private function set_matrixX(value : Float) : Float;
	private function set_matrixY(value : Float) : Float;
	private function set_preserveAlpha(value : Bool) : Bool;
}
