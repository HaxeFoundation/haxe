class Main {
	static function main() {
		#if fail
		foo(() -> () -> trace("whoops"));
		var cb:Void->Void = () -> function() {};
		foo(() -> {
			function() {};
		});
		#else
		foo(function() return function() {});
		var g = () -> function() {};
		foo(g);
		#end
	}

	static function foo(cb:Void->Void) {
		cb();
	}
}
