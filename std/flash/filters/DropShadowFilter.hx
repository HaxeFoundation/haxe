package flash.filters;

extern final class DropShadowFilter extends BitmapFilter {
	var alpha(get,set) : Float;
	var angle(get,set) : Float;
	var blurX(get,set) : Float;
	var blurY(get,set) : Float;
	var color(get,set) : UInt;
	var distance(get,set) : Float;
	var hideObject(get,set) : Bool;
	var inner(get,set) : Bool;
	var knockout(get,set) : Bool;
	var quality(get,set) : Int;
	var strength(get,set) : Float;
	function new(distance : Float = 4, angle : Float = 45, color : UInt = 0, alpha : Float = 1, blurX : Float = 4, blurY : Float = 4, strength : Float = 1, quality : Int = 1, inner : Bool = false, knockout : Bool = false, hideObject : Bool = false) : Void;
	private function get_alpha() : Float;
	private function get_angle() : Float;
	private function get_blurX() : Float;
	private function get_blurY() : Float;
	private function get_color() : UInt;
	private function get_distance() : Float;
	private function get_hideObject() : Bool;
	private function get_inner() : Bool;
	private function get_knockout() : Bool;
	private function get_quality() : Int;
	private function get_strength() : Float;
	private function set_alpha(value : Float) : Float;
	private function set_angle(value : Float) : Float;
	private function set_blurX(value : Float) : Float;
	private function set_blurY(value : Float) : Float;
	private function set_color(value : UInt) : UInt;
	private function set_distance(value : Float) : Float;
	private function set_hideObject(value : Bool) : Bool;
	private function set_inner(value : Bool) : Bool;
	private function set_knockout(value : Bool) : Bool;
	private function set_quality(value : Int) : Int;
	private function set_strength(value : Float) : Float;
}
