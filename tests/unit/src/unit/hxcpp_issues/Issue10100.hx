package unit.hxcpp_issues;

class Issue10100 extends Test {
	#if cpp
	@:analyzer(no_optimize)
	function test() {
		var cpp64:cpp.Int64 = untyped __cpp__('12345678944444');
		// https://repl.it/join/wtqnxkxc-haxiomic
		// low 1942935740
		// high 2874
		var hx64:haxe.Int64 = cpp64;
		eq(hx64.low, 1942935740);
		eq(hx64.high, 2874);

		// there and back again
		var cpp64:cpp.Int64 = hx64;
		eq(Std.string(cpp64), '12345678944444');

		var hx64:haxe.Int64 = cpp64;
		eq(hx64.low, 1942935740);
		eq(hx64.high, 2874);
	}
	#end
}
