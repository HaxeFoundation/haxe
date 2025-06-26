package cases;

class Issue10633 extends DisplayTestCase {
	/**
		enum Value {
			Multiple(a:Array<String>);
		}

		class Main {
			var values(get, set):Array<String>;

			var whatever:Value;

			function get_values() {
				return switch (whatever) {
					case Multiple(values): values;
					default: [];
				}
			}

			function set_values(a:Array<String>) {
				return [];
			}

			static function main() {}
		}
	**/
	function test() {
		arrayEq([], diagnostics());
	}
}
