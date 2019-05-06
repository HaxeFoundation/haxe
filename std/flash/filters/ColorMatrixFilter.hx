package flash.filters;

extern final class ColorMatrixFilter extends BitmapFilter {
	@:flash.property var matrix(get,set) : Array<Dynamic>;
	function new(?matrix : Array<Dynamic>) : Void;
	private function get_matrix() : Array<Dynamic>;
	private function set_matrix(value : Array<Dynamic>) : Array<Dynamic>;
}
