package flash.geom;

extern class PerspectiveProjection	{

	var projectionCenter : Point;
	var fieldOfView : Float;
	var focalLength(default,null) : Float;

	function new() : Void;
	function toMatrix3D() : Matrix3D;
}
