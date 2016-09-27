package unit.issues;

private typedef GroceryItem = { name: String }
private typedef FruitItem = { name: String, count: Int };

class Issue4086 extends Test {
	function test() {
		check(apply3("apple", 5));
	}

	static function check(item:GroceryItem) { }

	static inline function apply3(name: String, count: Int): FruitItem {
		return { name: name, count: count };
	}

}