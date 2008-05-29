package flash.geom;

extern class Matrix3D {

	var rawData : flash.Vector<Float>;
	var position : Vector3D;
	var determinant(default,null) : Float;

	function new( ?data : flash.Vector<Float> ) : Void;
	function transpose() : Void;
	function prependTranslation( x : Float, y : Float, z : Float ) : Void;
	function deltaTransformVector( v : Vector3D ) : Vector3D;
	function pointAt( ?pos : Vector3D, ?at : Vector3D, ?up : Vector3D ) : Void;
	function transformVectors( vin : flash.Vector<Float>, vout : flash.Vector<Float> ) : Void;
	function prependRotation( ?degrees : Float, ?axis : Vector3D, ?pivotPoint : Vector3D ) : Void;
	function prepend( rhs : Matrix3D ) : Void;
	function transformVector( v : Vector3D ) : Vector3D;
	function appendScale( xScale : Float, yScale : Float, zScale : Float ) : Void;
	function decompose( ?orientation : Orientation3D ) : flash.Vector<Vector3D>;
	function interpolateTo( toMat : Matrix3D, percent : Float ) : Void;
	function invert() : Bool;
	function appendTranslation( x : Float, y : Float, z : Float ) : Void;
	function appendRotation( ?degrees : Float, ?axis : Vector3D, ?pivotPoint : Vector3D ) : Void;
	function append( lhs : Matrix3D ) : Void;
	function prependScale( xScale : Float, yScale : Float, zScale : Float ) : Void;
	function clone() : Matrix3D;
	function identity() : Void;
	function recompose( ?components : flash.Vector<Vector3D>, ?orientation : Orientation3D ) : Bool;

	static function interpolate( thisMat : Matrix3D, toMat : Matrix3D, percent : Float ) : Matrix3D;
}
