package cases.display.issues;

class Issue12606 extends DisplayTestCase {
	/**
		class C {
			public function new() {}

			public function forwarded() {}
			public function notForwarded() {}
		}

		@:forward("forwarded")
		abstract A(C) {
			public function new() {
				this = new C();
			}
		}

		function main() {
			final a = new A();
			a.{-1-}
		}
	**/
	function test(_) {
		final fields = fields(1);
		eq(true, hasField(fields, "forwarded", "() -> Void"));
		eq(1, fields.length);
	}
}
