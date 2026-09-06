class Main {
	static function main():Void {
		var holder = new NativeCallbackHolder(cpp.Callable.fromStaticFunction(increment));
		if (holder.callback.call(41) != 42) {
			throw "Invalid native callback result";
		}
	}

	static function increment(value:Int):Int {
		return value + 1;
	}
}

class NativeCallbackHolder {
	public var callback:cpp.Function<Int->Int, cpp.abi.Abi>;

	public function new(callback:cpp.Function<Int->Int, cpp.abi.Abi>) {
		this.callback = callback;
	}
}
