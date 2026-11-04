package cases.display.issues;

class Issue2997 extends DisplayTestCase {
	/**
		enum abstract MyEnum(Int) {
		    var {-2-}A{-3-} = 1;
		    var B = 2;
		    var C = 3;
		}

		class Main {
		    static function main() {
		        var a = {-1-}A;
		        var b = MyEnum.B;
		    }
		}
	**/
	function testEnumAbstractFieldGotoDefinition(_) {
		Assert.same(range(2, 3), position(1));
	}
}
