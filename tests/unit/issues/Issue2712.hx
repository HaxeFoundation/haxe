package unit.issues;
import unit.Test;

class Item implements Heapable {}

interface Collection<T> {
    function contains(x:T):Bool;
}

interface Heapable {}

class Heap<T:(Heapable)> implements Collection<T> {
    public function new() {}

    public function contains(x:T):Bool {
        return true;
    }
}

class Issue2712 extends Test {
	function test() {
		var h = new Heap<Item>();
		eq(true, h.contains(null));
	}
}