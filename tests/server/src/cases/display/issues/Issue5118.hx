package cases.display.issues;

class Issue5118 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
		    static function main() {
		        var {-2-}a = Some(10);
		        switch (a) {
		            case None:
		            case {-1-}Some(v):
		        }
		    }
		}
	**/
	function testEnumTypes(_) {
		// hover on variable 'a'
		eq("haxe.ds.Option<Int>", type(2));
		// hover on 'Some' in case pattern - type should contain Option/Some info
		var t = type(1);
		Assert.isTrue(t.contains("Option") || t.contains("Some"), 'Expected Option/Some in type, got: $t');
	}
}
