class SortedStringMapImpl<V> extends haxe.ds.BalancedTree<String, V> implements haxe.Constraints.IMap<String,V> {

	var cmp:String -> String -> Int;

	public function new(?comparator:String -> String -> Int) {
		super();
		this.cmp = comparator == null ? haxe.Utf8.compare : comparator;
	}

	override
	function compare(s1:String, s2:String):Int {
		return cmp(s1, s2);
	}
}

class Main {
	static function main() {
		var m = new SortedStringMapImpl<String>();
		m.set("foo", "bar");
		trace(m);
	}
}
