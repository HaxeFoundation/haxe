package flash.geom;

@:require(flash10) extern class Utils3D {
	static function pointTowards(percent : Float, mat : Matrix3D, pos : Vector3D, ?at : Vector3D, ?up : Vector3D) : Matrix3D;
	static function projectVector(m : Matrix3D, v : Vector3D) : Vector3D;
	static function projectVectors(m : Matrix3D, verts : flash.Vector<Float>, projectedVerts : flash.Vector<Float>, uvts : flash.Vector<Float>) : Void;
}
