package flash.geom;

extern class Transform {
	@:flash.property var colorTransform(get,set) : ColorTransform;
	@:flash.property var concatenatedColorTransform(get,never) : ColorTransform;
	@:flash.property var concatenatedMatrix(get,never) : Matrix;
	@:flash.property var matrix(get,set) : Matrix;
	@:flash.property @:require(flash10) var matrix3D(get,set) : Matrix3D;
	@:flash.property @:require(flash10) var perspectiveProjection(get,set) : PerspectiveProjection;
	@:flash.property var pixelBounds(get,never) : Rectangle;
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
