package flash.geom;

extern class Transform {
	var colorTransform : ColorTransform;
	var concatenatedColorTransform(default,null) : ColorTransform;
	var concatenatedMatrix(default,null) : Matrix;
	var matrix : Matrix;
	@:require(flash10) var matrix3D : Matrix3D;
	@:require(flash10) var perspectiveProjection : PerspectiveProjection;
	var pixelBounds(default,null) : Rectangle;
	function new(displayObject : flash.display.DisplayObject) : Void;
	@:require(flash10) function getRelativeMatrix3D(relativeTo : flash.display.DisplayObject) : Matrix3D;
}
