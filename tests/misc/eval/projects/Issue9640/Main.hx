class Main {
	static function main() {
		var foo:Foo = null;
		var mono = new Mono(foo);
		$type(mono); // `Mono<Unknown<0>>` for both 4.1 and nightly.
		// var foolike:FooLike = mono.bound;
		// Uncommenting ^ would bind it correctly for both 4.1/nightly.
		var barlike:BarLike = mono.bound;
		// ^ 4.1 correctly errors out (not here, but in `new Mono(foo)`). Nightly happily compiles.
		$type(mono); // Both bind to `Mono<BarLike>`.
	}
}

class Mono<A> {
	public final bound:A;
	public function new<B:A>(obj:B) bound = obj;
}

class Foo { public function foo():Void {} }
typedef FooLike = { function foo():Void; }
typedef BarLike = { function bar():Void; }