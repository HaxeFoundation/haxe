package issues;

#if !macro
import TestJs.use;
#end

@:callable
abstract From<T>(T) from T to T {
	inline function new (t:T) return new From(t);
	@:from macro public static function fromExpr (e:haxe.macro.Expr) {
		return macro function (a, b) return a+b;
	}
}

class Issue6283 {
	@:js('TestJs.use(3);')
	@:analyzer(no_local_dce)
	static function f(a, b) {
		#if !macro
		use(foo({}));
		#end
	}

	static inline function foo(f:From<Int->Int->Int>) {
		return f(1,2);
	}
}