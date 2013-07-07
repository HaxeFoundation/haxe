package flash.geom;

@:require(flash10) extern class PerspectiveProjection {
	var fieldOfView : Float;
	var focalLength : Float;
	var projectionCenter : Point;
	function new() : Void;
	function toMatrix3D() : Matrix3D;
}
