package unit.issues;

class Issue7661 extends unit.Test {
	function test() {
		var result = [for(i in new AbstractIterator(0, 3)) i];
		aeq([0, 1, 2], result);
	}
}

abstract AbstractIterator({current:Int, last:Int}) {
	public var length(get,never):Int;
	inline function get_length() return this.last - this.current;

	@:arrayAccess
	public function get(i:Int) return this.current - i;

	public inline function new(start:Int, end:Int) {
		this = {current:start, last:end};
	}

	public inline function hasNext()
		return this.current < this.last;

	public inline function next()
		return this.current++;
}