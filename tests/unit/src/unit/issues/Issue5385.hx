package unit.issues;

class Issue5385 extends unit.Test {
	function test() {
		var refs:Refs = ["1"];
		var myIter:MyIterator<String> = refs;
		t(myIter.hasNext());
		eq("1", myIter.next());
	}
}

private abstract MyIterator<T>(Iterator<T>) from Iterator<T> to Iterator<T> {
	@:from public static inline function fromIterable<T>(i:Iterable<T>)
		return (i.iterator():MyIterator<T>);

	public inline function hasNext() {
		return this.hasNext();
	}

	public inline function next() {
		return this.next();
	}
}

@:forward
@:transitive
private abstract Refs(Array<String>) from Array<String> to Array<String> {}
