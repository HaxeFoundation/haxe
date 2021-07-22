class Main {
	static function main() {
		try {
			test();
		}
		catch (e:Dynamic) {
			var stack = haxe.CallStack.exceptionStack();
			if(stack.length == 0) {
				throw 'haxe.CallStack.exceptionStack() returned empty array';
			}
		}
	}

	static public function test() {
		throw 'sdffsd';
	}
}