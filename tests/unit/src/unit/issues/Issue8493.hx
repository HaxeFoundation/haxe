package unit.issues;

#if flash
import flash.Vector;
#end

class Issue8493 extends unit.Test {
	#if flash
	static var v = new Vector<Vector<Array<Int>>>();
	static var typeRef = (Vector.typeReference() : Class<Vector<Vector<Array<Int>>>>);

	function test() {
		t(Std.isOfType(v, (Vector.typeReference() : Class<Vector<Vector<Array<Int>>>>)));
		t(Std.downcast(v, (Vector.typeReference() : Class<Vector<Vector<Array<Int>>>>)) == v);
		t(flash.Lib.as(v, (Vector.typeReference() : Class<Vector<Vector<Array<Int>>>>)) == v);

		t(Std.isOfType(v, typeRef));
		t(Std.downcast(v, typeRef) == v);
		t(flash.Lib.as(v, typeRef) == v);
	}
	#end
}