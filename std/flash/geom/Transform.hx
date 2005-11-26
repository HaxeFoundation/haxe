package flash.geom;

extern class Transform {

	var matrix : Matrix;
	var concatenatedMatrix : Matrix;
	var colorTransform : ColorTransform;
	var concatenatedColorTransform : ColorTransform;
	var pixelBounds : Rectangle<Float>;

	function new( mc : MovieClip ) : Void;

}