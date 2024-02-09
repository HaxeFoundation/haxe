#if !macro @:build(Macro.logBuild()) #end
class Bar {
	static function __init__() Sys.println("[runtime] Hello from Bar");
}

typedef B = Foo<Bar>;
