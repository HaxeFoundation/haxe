package cases;

class Issue6068 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var a:{i:Int};
				a({-1-});

				Main({-2-});
			}
		}
	**/
	function test() {
		check(function() signature(pos(1)));
		check(function() signature(pos(2)));
	}

	function check(fn) {
		var result = try {
			fn();
			false;
		} catch (e:HaxeInvocationException) {
			e.message.indexOf("Not a callable type") != -1;
		}
		assert(result);
	}
}
