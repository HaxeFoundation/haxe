package unit.issues;

class Issue5890 extends unit.Test {
	function test() {
		new ReporterImplSuc2();
		noAssert();
	}
}

interface Reporter<TDef> {
	public function shoot<T:TDef>(ctx:T):Void;
}

class ReporterImpl<A, TDef> implements Reporter<TDef> {
	public function shoot<T:TDef>(ctx:T):Void {}
}

class Impl2 implements Reporter<Int> {
	public function shoot<T:Int>(a:T) {}
}

class ReporterImplSuc<TDef, A> extends ReporterImpl<A, TDef> implements Reporter<TDef> {}

class ReporterImplSuc2 extends ReporterImpl<Int, String> implements Reporter<String> {
	public function new() {}
}

class HierarchyImpl implements ISuc<Float> {
	public function shoot<A:Float>(a:A) {
		trace(a + 5);
	}
}

class HierarchyImpl2<T> implements ISuc<T> {
	public function shoot<A:T>(a:A) {}
}

interface IBase2<T> {
	function shoot<A:T>(a:A):Void;
}

interface OtherBase {}
interface ISuc<T> extends IBase2<T> extends OtherBase {}
