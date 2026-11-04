package cases;

// Recursive anonymous structures produce cyclic HVirtual types. Besides
// `anons_cache`/`gather_types`, genhl keys other caches on `ttype`; if those use
// polymorphic compare they loop forever on two distinct-but-parallel cyclic
// virtuals. These are compile-only regression tests: if a cache loops, HL
// generation never finishes (Out of memory), so merely compiling this file
// exercises the fix.

private typedef Foo1 = {
	function make<T>(v:T):Foo1;
}

private typedef Foo2 = {
	function make<T>(v:T):Foo2;
}

class RecursiveAnonCaches {
	// Closures capturing a recursive-anon variable (plus one more captured var)
	// pack their captures into a tuple -> genhl `cached_tuples`.
	static public function captureTuples() {
		var a:Foo1 = null;
		var b:Foo2 = null;
		var n = 1;
		var f = function() return a != null && n > 0;
		var g = function() return b != null && n > 0;
		f();
		g();
	}
}
