package flash.geom;

@:require(flash10) extern class Matrix3D {
	var determinant(default,never) : Float;
	var position : Vector3D;
	var rawData : flash.Vector<Float>;
	function new(?v : flash.Vector<Float>) : Void;
	function append(lhs : Matrix3D) : Void;
	function appendRotation(degrees : Float, axis : Vector3D, ?pivotPoint : Vector3D) : Void;
	function appendScale(xScale : Float, yScale : Float, zScale : Float) : Void;
	function appendTranslation(x : Float, y : Float, z : Float) : Void;
	function clone() : Matrix3D;
	@:require(flash11) function copyColumnFrom(column : UInt, vector3D : Vector3D) : Void;
	@:require(flash11) function copyColumnTo(column : UInt, vector3D : Vector3D) : Void;
	@:require(flash11) function copyFrom(sourceMatrix3D : Matrix3D) : Void;
	@:require(flash11) function copyRawDataFrom(vector : flash.Vector<Float>, index : UInt = 0, transpose : Bool = false) : Void;
	@:require(flash11) function copyRawDataTo(vector : flash.Vector<Float>, index : UInt = 0, transpose : Bool = false) : Void;
	@:require(flash11) function copyRowFrom(row : UInt, vector3D : Vector3D) : Void;
	@:require(flash11) function copyRowTo(row : UInt, vector3D : Vector3D) : Void;
	@:require(flash11) function copyToMatrix3D(dest : Matrix3D) : Void;
	function decompose(?orientationStyle : Orientation3D) : flash.Vector<Vector3D>;
	function deltaTransformVector(v : Vector3D) : Vector3D;
	function identity() : Void;
	function interpolateTo(toMat : Matrix3D, percent : Float) : Void;
	function invert() : Bool;
	function pointAt(pos : Vector3D, ?at : Vector3D, ?up : Vector3D) : Void;
	function prepend(rhs : Matrix3D) : Void;
	function prependRotation(degrees : Float, axis : Vector3D, ?pivotPoint : Vector3D) : Void;
	function prependScale(xScale : Float, yScale : Float, zScale : Float) : Void;
	function prependTranslation(x : Float, y : Float, z : Float) : Void;
	function recompose(components : flash.Vector<Vector3D>, ?orientationStyle : Orientation3D) : Bool;
	function transformVector(v : Vector3D) : Vector3D;
	function transformVectors(vin : flash.Vector<Float>, vout : flash.Vector<Float>) : Void;
	function transpose() : Void;
	static function interpolate(thisMat : Matrix3D, toMat : Matrix3D, percent : Float) : Matrix3D;
}
