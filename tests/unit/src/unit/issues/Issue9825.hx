package unit.issues;

typedef KnownDynamic<T> = {known:T} &
	Dynamic<T>;

class Issue9825 extends Test {
	function test() {
		var d:KnownDynamic<String> = {known: "foo", also: "bar"};
		eq("foo", d.known);
		eq("bar", d.also);
		d.even = "even";
		eq("even", d.even);
		t(HelperMacros.typeError(d.no = 12));
	}
}
