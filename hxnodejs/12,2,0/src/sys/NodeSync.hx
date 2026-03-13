package sys;

@:jsRequire('deasync')
private extern class Deasync {
	public static function loopWhile(f:Void->Bool):Void;
}

class NodeSync {
	public static function callMany(f:Dynamic->Void):Array<Dynamic> {
		var retArgs = null;
		var wait = Reflect.makeVarArgs(function(args) retArgs = args);
		f(wait);
		Deasync.loopWhile(function() return retArgs == null);
		return retArgs;
	}

	public static function callVoid(f:(Void->Void)->Void) {
		var called = false;
		f(function() called = true);
		Deasync.loopWhile(function() return !called);
	}

	public static inline function wait(f:Void->Bool) {
		Deasync.loopWhile(function() return !f());
	}
}
