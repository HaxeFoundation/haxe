// Create a dependency to Bar
import Bar;

typedef A = Foo<Main>;

#if !macro @:build(Macro.logBuild()) #end
class Main {
	static function main() Sys.println("[runtime] Hello from Main");
}
