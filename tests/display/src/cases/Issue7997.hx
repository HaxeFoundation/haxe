package cases;

class Issue7997 extends DisplayTestCase {
	/**
		enum Dummy {
			One;
			Two;
			Three;
		}

		class Main {
			static function main() {}

			function guessName<T>():Null<String> {
				function loop(type) {
					return switch (type.kind) {
						case One: type.args.path.typeName;
						case Two: loop(type.args);
						case _: null;
					}
				}
				return lo{-1-}op(null);
			}
		}
	**/
	function testAbstractShadowsForwardedField() {
		var sig = "type:{ path : { typeName : Unknown<0> }, kind : cases.Dummy, args : { path : { typeName : Unknown<0> }, kind : cases.Dummy, args : { *RECURSION* : ? } } }";
		sigEq(0, [[sig]], signature(pos(1)));
	}
}
