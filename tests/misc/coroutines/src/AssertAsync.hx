import haxe.coro.Coroutine;

import haxe.ValueException;

class AssertAsync {
	@:coroutine
	static function _raisesImpl(method:Coroutine<() -> Void>, type:Any) {
		var typeDescr = type != null ? "exception of type " + Type.getClassName(type) : "exception";

		try {
			method();
		} catch (ex:Dynamic) {
			var ex = Std.isOfType(ex, ValueException) ? (cast ex:ValueException).value : (ex:Any);

			return Assert.isTrue(Std.isOfType(ex, type), "expected " + typeDescr + " but it is "  + ex);
		}

		return Assert.fail('Exception not thrown');
	}

	@:coroutine
	public static function raises(method:Coroutine<() -> Void>, type:Any) : Bool {
		return _raisesImpl(method, type);
	}
}