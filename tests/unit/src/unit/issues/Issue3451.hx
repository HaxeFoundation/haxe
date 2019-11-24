package unit.issues;

#if flash
class MatrixExt extends flash.geom.Matrix { }
#end

class Issue3451 extends Test {
	#if flash
	public function test() {
		var m = new MatrixExt();
		feq(1, m.a);
		feq(0, m.b);
		feq(0, m.c);
		feq(1, m.d);
		feq(0, m.tx);
		feq(0, m.ty);
	}
	#end
}