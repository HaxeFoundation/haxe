package flash.geom;

extern class Transform {
	var colorTransform(get,set) : ColorTransform;
	var concatenatedColorTransform(get,never) : ColorTransform;
	var concatenatedMatrix(get,never) : Matrix;
	var matrix(get,set) : Matrix;
	@:require(flash10) var matrix3D(get,set) : Matrix3D;
	@:require(flash10) var perspectiveProjection(get,set) : PerspectiveProjection;
	var pixelBounds(get,never) : Rectangle;
	function new(displayObject : flash.display.DisplayObject) : Void;
	@:require(flash10) function getRelativeMatrix3D(relativeTo : flash.display.DisplayObject) : Matrix3D;
	private function get_colorTransform() : ColorTransform;
	private function get_concatenatedColorTransform() : ColorTransform;
	private function get_concatenatedMatrix() : Matrix;
	private function get_matrix() : Matrix;
	private function get_matrix3D() : Matrix3D;
	private function get_perspectiveProjection() : PerspectiveProjection;
	private function get_pixelBounds() : Rectangle;
	private function set_colorTransform(value : ColorTransform) : ColorTransform;
	private function set_matrix(value : Matrix) : Matrix;
	private function set_matrix3D(value : Matrix3D) : Matrix3D;
	private function set_perspectiveProjection(value : PerspectiveProjection) : PerspectiveProjection;
}
