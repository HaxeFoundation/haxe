class Main {
	static function main() {
		A;
		B;
	}
}

@:autoBuild(Macro.build(1))
@:autoBuild(Macro.build(2))
interface IFoo {}

class A implements IFoo {}

@:build(Macro.build(3))
@:build(Macro.build(4))
class B {}
