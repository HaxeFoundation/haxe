package cases.display.issues;

class Issue5134 extends DisplayTestCase {
	/**
		@:generic
		class A<T> {
		    var items:Array<T>;
		    public function new() {
		        items = [];
		    }
		    inline public function get(_index:Int) : T {
		        return items[_index];
		    }
		}

		class Main {
		    var a: A<Int>;

		    function new() {}

		    static function main() {
		        trace("Haxe is great!");
		        a.get(0);
		    }
		}
	**/
	function testGenericClassCompletionError(_) {
		// Accessing instance field 'a' in static context should give a type error
		runHaxe(["-main", "Main"]);
		assertErrorMessage("Cannot access a in static function");
	}
}
