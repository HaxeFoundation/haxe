package issues;

typedef List = { v : Int, next : List };

class Issue3524 {
	@:js('
		var l1_v;
		var l1_next;
		l1_v = 0;
		l1_next = null;
		var l2_v;
		var l2_next;
		l2_v = 1;
		l2_next = null;
		l1_v - l2_v;
	')
	@:analyzer(ignore)
	static function main() {
		var l1 = { v: 0, next: null };
		var l2 = { v: 1, next: null };
		apply(cmp, l1, l2);
	}

	static inline function cmp(a:List, b:List) {
		return a.v - b.v;
	}

	static inline function apply(f:List -> List -> Int, l1:List, l2:List) {
		f(l1, l2);
	}
}