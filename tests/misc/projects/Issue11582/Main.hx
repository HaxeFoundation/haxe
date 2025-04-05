class Main {
	static function main() {
		trace("Foo");
		trace(Foo1.macros);
		trace(Foo2.macros);
		trace(Foo3.macros);
		trace(Foo4.macros);

		trace("Bar");
		trace(Bar1.macros);
		trace(Bar2.macros);
		trace(Bar3.macros);

		trace("Baz");
		trace(Baz1.macros);
		trace(Baz2.macros);
		trace(Baz3.macros);
	}
}

@:autoBuild(Macro.build("I1"))
interface I1 {}

@:autoBuild(Macro.build("I2"))
interface I2 {}

@:autoBuild(Macro.build("auto Foo1 (1)"))
@:autoBuild(Macro.build("auto Foo1 (2)"))
@:build(Macro.build("Foo1"))
class Foo1 implements I1 implements I2 {}

@:build(Macro.build("Foo2"))
class Foo2 extends Foo1 {}

@:build(Macro.build("Foo3 (1)"))
@:build(Macro.build("Foo3 (2)"))
class Foo3 extends Foo2 {}

@:build(Macro.build("Foo4"))
class Foo4 extends Foo3 {}

class Bar1 implements I2 implements I1 {}
class Bar2 extends Bar1 {}
class Bar3 extends Bar2 {}

@:autoBuild(Macro.build("I3"))
interface I3 extends I1 {}

class Baz1 implements I3 implements I2 {}
class Baz2 extends Baz1 {}
class Baz3 extends Baz2 {}
