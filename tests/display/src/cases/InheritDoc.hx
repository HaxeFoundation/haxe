package cases;

using StringTools;

class InheritDoc extends DisplayTestCase {
	/**
		import misc.InheritDocTypes;
		class Main {
			static public function main() {
				var c = new Child();
				c.{-1-}
			}
		}
	**/
	function testSuperMethod() {
		var test = getField(pos(1), 'test');
		eq(' Child doc \n GrandParent doc ', test.doc);
	}
}
