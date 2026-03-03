package cases.display.issues;

class Issue3310 extends DisplayTestCase {
	/**
		abstract A(Int) {}

		class Main {
		    static function main() { {-1-} }
		}
	**/
	function testAbstractImplNotInCompletion(_) {
		var items = toplevel(1);
		for (item in items) {
			switch item.kind {
				case Type:
					Assert.isFalse(item.args.path.typeName.indexOf("_Impl_") != -1,
						'Abstract implementation class ${item.args.path.typeName} should not appear in completion');
				case _:
			}
		}
	}
}
