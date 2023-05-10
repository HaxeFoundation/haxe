package cases;

class Foo {
	public final field = 0;

	public function new() {}
}

class Issue11173 extends DisplayTestCase {
	/**
		class Main {
			static final {-1-}field{-2-} = 0;
			static function main() {
				{-3-}field{-4-} = 5;

				final foo = new Foo();
				foo.{-5-}field{-6-} = "ho${-9-}la";
			}
		}
		class Foo {
			public final {-7-}field{-8-} = "hi";
			public function new() {}
		}
	**/
	function test() {
		arrayEq([
			{
				kind: DKCompilerError,
				severity: Error,
				code: null,
				range: diagnosticsRange(pos(3), pos(4)),
				relatedInformation: [],
				args: "This expression cannot be accessed for writing"
			},
			{
				kind: DKCompilerError,
				severity: Error,
				code: null,
				range: diagnosticsRange(pos(5), pos(6)),
				relatedInformation: [],
				args: "This expression cannot be accessed for writing"
			}
		], diagnostics());
		eq("Int", type(pos(4)));
		eq("String", type(pos(6)));
		eq(range(1, 2), position(pos(4)));
		eq(range(7, 8), position(pos(6)));
		eq("String", type(pos(9)));
	}
}
