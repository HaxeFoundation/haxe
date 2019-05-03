package flash.geom;

@:require(flash10) extern class PerspectiveProjection {
	var fieldOfView(get,set) : Float;
	var focalLength(get,set) : Float;
	var projectionCenter(get,set) : Point;
	function new() : Void;
	private function get_fieldOfView() : Float;
	private function get_focalLength() : Float;
	private function get_projectionCenter() : Point;
	private function set_fieldOfView(value : Float) : Float;
	private function set_focalLength(value : Float) : Float;
	private function set_projectionCenter(value : Point) : Point;
	function toMatrix3D() : Matrix3D;
}
