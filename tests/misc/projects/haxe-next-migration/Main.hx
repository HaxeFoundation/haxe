import pack.Mod;
import pack.E;
import pack.Thing;
import pack.E in Alias;

class Main {
	static function main() {
		var s:pack.SubType = new pack.SubType();
		trace(pack.SubType.x);
		trace(Thing);
		trace(Other);
		trace(Local);
		trace(Alias.Other);
		var c = sys.db.Sqlite.open(":memory:");
		Shadowed.test();
	}

	static function args(a:Int, a:Int, _:Int, _:Int) {}
}

class Local {}

enum LocalEnum {
	Local;
}

class Qualified extends pack.SubType {}
class Imported extends SubType {}

class A extends Base {
	function render() "a";
}

class B extends Base {
	function render() "b";
}

class C extends Quiet {}
