package cases.display.issues;

class Issue5152 extends DisplayTestCase {
	/**
		class Main {
		    static function main() {
		        var who = "world";
		        {-1-}'hello, $who, how are you';
		    }
		}
	**/
	function testStringInterpolationType(_) {
		eq("String", type(1));
	}
}
