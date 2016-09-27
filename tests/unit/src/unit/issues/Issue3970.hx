package unit.issues;

private enum DataType<T> {
	leaf(v:T);
	node(v:T,cs:Array<DataType<T>>);
}

class Issue3970 extends Test {
	static function fold<T,U>(f:T->DataType<U>->T,acc:T,v:DataType<U>):T return switch v {
		case node(v,cs):
			var r = acc;
			for(c in cs) r = acc = f(acc,c);
				r;
		case _:acc;
	}

	function test() {
		var nodes = node(1,[node(2,[node(3,[leaf(1)]),node(4,[leaf(2)])]),node(5,[leaf(3)])]);
		function f(acc:Int,v) return switch v {
			case node(x,cs): fold(f,acc+x,v);
			case leaf(x):    acc + x;
		}
		var r = f(0,nodes);
		eq(21, r);
	}
}