package unit.issues;

import haxe.ds.Either;

private abstract PhysicalType<T>(Either<Class<T>, Enum<T>>) {

	public function new(v) {
		this = v;
	}

	public function toString() {
		return switch this {
			case Left(c): Type.getClassName(c);
			case Right(e): Type.getEnumName(e);
		}
	}

	static public function fromClass<T>(c:Class<T>) {
		return new PhysicalType(Left(c));
	}

	static public function fromEnum<T>(en:Enum<T>) {
		return new PhysicalType(Right(en));
	}
}

class Issue4966 extends Test {

	function test() {
		eq("type: unit.Test", throws(PhysicalType.fromClass(Test)));
		eq("type: haxe.macro.ExprDef", throws(PhysicalType.fromEnum(haxe.macro.Expr.ExprDef)));
	}

	function throws<A>(t:PhysicalType<A>) {
		return 'type: $t';
	}
}