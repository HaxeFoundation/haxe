package flash.filters;

extern class ColorMatrixFilter extends BitmapFilter {

	var matrix : Array<Float>; // 20 Numbers

	function new( ?matrix : Array<Dynamic> ) : Void;
	function clone() : ColorMatrixFilter;

}
