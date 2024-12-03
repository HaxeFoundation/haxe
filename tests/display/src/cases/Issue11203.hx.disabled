package cases;

class Issue11203 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var future = new Future();
				future.eager();
			}
		}

		abstract Future({}) from {} {
			public function new() this = {};

			public inline function eager():Future {
				trace("much side effect!");
				return this;
			}
		}
	**/
	function test() {
		arrayEq([], diagnostics());
	}
}
