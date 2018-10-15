package cases;

class Issue4345 extends DisplayTestCase {
	/**
	typedef DTag = Tag<DTag>;  // kind of a defeat, but OK..
	typedef Tag1 = { tag: TagE<Tag1>, name: String, count: Int }
	typedef Tag2 = { tag: TagE<Tag2>, name: String, brand: String, liters: Float};

	@:enum abstract TagE<T>(Int) {
		var fruit:   TagE<Tag1> = 1;
		var beverage:TagE<Tag2> = 2;
	}

	typedef Tag<T> = { tag:TagE<T> };

	class Test {

		static var data : Array<Dynamic> =
		[
			{ tag: fruit,    name: "Apple",  count: 5 },
			{ tag: fruit,    name: "Orange", count: 3 },
			{ tag: beverage, name: "Sprite", brand: "Coca-Cola", liters: 2.5 }
		];

		static function untag<T:Tag<T>>(t:T) switch t.tag {
			case fruit:
				t.{-1-}
		}
	**/
	function test() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "name", "String"));
		eq(true, hasField(fields, "count", "Int"));
	}
}