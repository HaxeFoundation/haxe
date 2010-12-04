package flash.geom;

extern class Transform {
	var colorTransform : ColorTransform;
	var concatenatedColorTransform(default,null) : ColorTransform;
	var concatenatedMatrix(default,null) : Matrix;
	var matrix : Matrix;
	var pixelBounds(default,null) : Rectangle;
	function new(displayObject : flash.display.DisplayObject) : Void;
}
