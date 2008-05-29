package flash.geom;

extern class Transform {
	function new(displayObject : flash.display.DisplayObject) : Void;
	var colorTransform : flash.geom.ColorTransform;
	var concatenatedColorTransform(default,null) : flash.geom.ColorTransform;
	var concatenatedMatrix(default,null) : flash.geom.Matrix;
	var matrix : flash.geom.Matrix;
	var pixelBounds(default,null) : flash.geom.Rectangle;
	#if flash10
	var matrix3D : flash.geom.Matrix3D;
	var perspectiveProjection : flash.geom.PerspectiveProjection;
	function getRelativeMatrix3D( relativeTo:flash.display.DisplayObject ) : Matrix3D;
	#end
}
