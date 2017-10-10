package unit.issues;

class Issue6637 extends unit.Test {
	function test() {
		//this is required to enable HxOverrides.iter on js
		([]:Iterable<Int>);

		var value = {iterator:10};
		decrease(value);

		eq(value.iterator, 8);
	}

	static public function decrease<T:{iterator:Int}>(v:T) {
		v.iterator--;
		--v.iterator;
	}
}
