package unit.issues;

private enum E1 {
	A;
}

private enum E2 {
	B;
}

private enum E3 {
	C(str:String);
}

@:structInit
private class TVar {
	public var id : Int;
	public var type : TType;
	public var kind : VarKind;
}

private enum VarKind {
	Global;
	Input;
	Param;
}

private enum TType {
	TVoid;
	TArray( t : TType );
	TSampler( dim : TexDimension );
}

enum TexDimension {
	T1D;
	T2D;
	T3D;
	TCube;
}

class Issue12538 extends Test {
	function test() {
		var map = new haxe.ds.ObjectMap();
		var a : Dynamic = A;
		var b : Dynamic = B;
		var c1 : Dynamic = C("1");
		var c2 : Dynamic = C("2");
		map.set(a, 1);
		eq(1, map.get(a));
		eq(null, map.get(b));
		map.set(c1, 2);
		eq(1, map.get(a));
		eq(null, map.get(b));
		eq(2, map.get(c1));
		eq(null, map.get(c2));
	}

	function testCopy() {
		var v : TVar = {
			id : 0,
			type : TArray(TSampler(T3D)),
			kind : Param,
		};
		var v2 = haxe.runtime.Copy.copy(v);
		t(v != v2);
		eq(v.kind, v2.kind);
	}
}
