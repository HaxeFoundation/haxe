package cases;

class Issue6923 extends DisplayTestCase {
	/**
	class Main {
		static function main() {
			trace({-1-}"test{-2-}".{-3-}lengt{-4-}h{-5-});
		}
	}
	**/
	function test() {
		eq("String", type(pos(1)));
		eq("String", type(pos(2)));
		eq("Int", type(pos(3)));
		eq("Int", type(pos(4)));
		eq("Void", type(pos(5)));
	}

	/**
	class Main {
		static function main() {
			var a = 1;
			{-1-}a{-2-}+{-3-}0{-4-}.{-5-}1;
		}
	}
	**/
	function test2() {
		eq("Int", type(pos(1)));
		eq("Float", type(pos(2)));
		eq("Float", type(pos(3)));
		eq("Float", type(pos(4)));
		eq("Float", type(pos(5)));
	}
}
