class Main {
	public static function main() {
		var x:cs.NativeArray<Int> = new cs.NativeArray(1);

		cs.Lib.unsafe({trace(42);});
		cs.Lib.unsafe(trace(42));

		cs.Lib.unsafe({
			cs.Lib.fixed({
				var addr = cs.Lib.pointerOfArray(x);
				trace(cs.Lib.valueOf(addr)); //0
				addr[0] = 42;
				trace(cs.Lib.valueOf(addr)); //42
			});
		});
	}

	@:unsafe static function unsafeFunction() {}
}

@:unsafe
class TestUnsafe {}
