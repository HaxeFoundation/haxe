package flash.geom;

#if !flash8
"This class is only accesible in Flash8"
#end

extern class Transform {

	var matrix : Matrix;
	var concatenatedMatrix : Matrix;
	var colorTransform : ColorTransform;
	var concatenatedColorTransform : ColorTransform;
	var pixelBounds : Rectangle<Float>;

	function new( mc : flash.MovieClip ) : Void;

}