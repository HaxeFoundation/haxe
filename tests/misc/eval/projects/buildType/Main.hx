class Main {
	static function main() {
		trace(Base.info());
		trace(Reader.info());
	}
}

// Its build macro adds @:built, and reads the header of Child, which extends Base:
// this does not build Child, so it is not a cycle.
@:build(Macro.addMeta())
class Base {}

class Child extends Base {}

@:build(Macro.readMeta())
class Reader {}
