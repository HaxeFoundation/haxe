package unit.issues;

typedef KnownDynamic<T> = {known:T} &
	Dynamic<T>;

class Issue9825 extends Test {
	function test() {
		var d:KnownDynamic<String> = {known: "foo", also: "bar"};
	}
}
