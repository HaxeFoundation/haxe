package haxe.coro;

/**
	Coroutine is the type of functions that are coroutines, which the compiler
	treats in a special way. The type parameter `T` represents the signature of
	the function itself.

	The compiler automatically uses this type for any `@:coroutine function`:

	```haxe
	class C {
		@:coroutine static function staticCoro() {}
		@:coroutine function memberCoro(s:String) {}

		public function new() {
			$type(staticCoro); // haxe.coro.Coroutine<() -> Void>
			$type(memberCoro); // haxe.coro.Coroutine<(s : String) -> Void>
			$type(@:coroutine function localCoro() return "foo"); // haxe.coro.Coroutine<() -> String>
		}
	}
	```

	The `@:coroutine` metadata can be omitted when the expected type is `Coroutine`:

	```haxe
	function expectCoro(c:haxe.coro.Coroutine<Int->String>) {}
	expectCoro(function(i:Int) return Std.string(i));
	```
**/
@:callable
@:coreType
abstract Coroutine<T:haxe.Constraints.Function> { }
