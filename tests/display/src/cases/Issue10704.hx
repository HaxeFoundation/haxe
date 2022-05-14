package cases;

class Issue10704 extends DisplayTestCase {
	/**
		import misc.issue10704.Statics.*;
		class Main {
			static function main() {
				foo{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasToplevel(toplevel(pos(1)), "static", "fooPublic"));
		eq(false, hasToplevel(toplevel(pos(1)), "static", "fooNoCompletion"));
		eq(false, hasToplevel(toplevel(pos(1)), "static", "fooPrivate"));
	}
}
