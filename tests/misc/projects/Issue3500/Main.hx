class Main {
	static function test() {
		function fail(msg) {
			Sys.println(msg);
			Sys.exit(1);
		}

		haxe.macro.Context.onAfterInitMacros(() -> {
			var t = haxe.macro.Context.getType("A");
			switch (t) {
				case TAbstract(a, _):
					var hasTestMeta = Lambda.exists(a.get().impl.get().meta.get(), function(m) return m.name == ":test");
					if (!hasTestMeta) {
						fail("Abstract implementation class has no @:test metadata");
					}
				case _:
					fail("Should be abstract");
			}
		});
	}
}
