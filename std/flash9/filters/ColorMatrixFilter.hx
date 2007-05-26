package flash.filters;

extern class ColorMatrixFilter extends flash.filters.BitmapFilter {
	function new(?matrix : Array<Dynamic>) : Void;
	var matrix : Array<Float>;
}
