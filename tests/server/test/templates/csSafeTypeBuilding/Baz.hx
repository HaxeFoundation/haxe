#if !macro @:build(Macro.logBuild()) #end
class Baz {
	static function __init__() Sys.println("[runtime] Hello from Baz");
}

typedef AA = Foo<Bar>;
typedef BB = Foo<Baz>;
