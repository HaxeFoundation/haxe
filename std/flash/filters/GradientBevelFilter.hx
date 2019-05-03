package flash.filters;

extern final class GradientBevelFilter extends BitmapFilter {
	var alphas(get,set) : Array<Dynamic>;
	var angle(get,set) : Float;
	var blurX(get,set) : Float;
	var blurY(get,set) : Float;
	var colors(get,set) : Array<Dynamic>;
	var distance(get,set) : Float;
	var knockout(get,set) : Bool;
	var quality(get,set) : Int;
	var ratios(get,set) : Array<Dynamic>;
	var strength(get,set) : Float;
	var type(get,set) : String;
	function new(distance : Float = 4, angle : Float = 45, ?colors : Array<Dynamic>, ?alphas : Array<Dynamic>, ?ratios : Array<Dynamic>, blurX : Float = 4, blurY : Float = 4, strength : Float = 1, quality : Int = 1, ?type : String, knockout : Bool = false) : Void;
	private function get_alphas() : Array<Dynamic>;
	private function get_angle() : Float;
	private function get_blurX() : Float;
	private function get_blurY() : Float;
	private function get_colors() : Array<Dynamic>;
	private function get_distance() : Float;
	private function get_knockout() : Bool;
	private function get_quality() : Int;
	private function get_ratios() : Array<Dynamic>;
	private function get_strength() : Float;
	private function get_type() : String;
	private function set_alphas(value : Array<Dynamic>) : Array<Dynamic>;
	private function set_angle(value : Float) : Float;
	private function set_blurX(value : Float) : Float;
	private function set_blurY(value : Float) : Float;
	private function set_colors(value : Array<Dynamic>) : Array<Dynamic>;
	private function set_distance(value : Float) : Float;
	private function set_knockout(value : Bool) : Bool;
	private function set_quality(value : Int) : Int;
	private function set_ratios(value : Array<Dynamic>) : Array<Dynamic>;
	private function set_strength(value : Float) : Float;
	private function set_type(value : String) : String;
}
