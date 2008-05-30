package flash.geom;

extern class Transform {
	var colorTransform : ColorTransform;
	var concatenatedColorTransform(default,null) : ColorTransform;
	var concatenatedMatrix(default,null) : Matrix;
	var matrix : Matrix;
	var pixelBounds(default,null) : Rectangle;
	function new(displayObject : flash.display.DisplayObject) : Void;
	#if flash10
	var matrix3D : flash.geom.Matrix3D;
	var perspectiveProjection : flash.geom.PerspectiveProjection;
	function getRelativeMatrix3D( relativeTo:flash.display.DisplayObject ) : Matrix3D;
	#end
}
