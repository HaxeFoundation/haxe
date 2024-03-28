package haxe.coro;

/**
	Coroutine function.
**/
@:callable
@:coreType
abstract Coroutine<T:haxe.Constraints.Function> {
	/**
		Suspend running coroutine and expose the continuation callback
		for resuming coroutine execution.
	**/
	@:coroutine
	#if cpp
	@:native("::hx::Coroutine::suspend")
	#end
	public static extern function suspend<T>(f:(cont:Continuation<T, Null<Dynamic>>)->Void):T;

	#if (jvm || eval || hl)
	@:native("suspend")
	@:keep
	static function nativeSuspend<T>(f, cont:Continuation<T, Null<Dynamic>>) {
		return (_, _) -> f(cont);
	}
	#end

	#if js // TODO: implement this all properly for all the targets
	static function __init__():Void {
		js.Syntax.code("{0} = {1}", Coroutine.suspend, cast function(f, cont) return (_, _) -> f(cont));
	}
	#end
}
