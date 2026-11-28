package cases.display;

class CallTypeArguments extends DisplayTestCase {
	/**
		class Main {
			static function make<T>():Array<T> return [];
			static function main() {
				make.<{-1-}
			}
		}
		typedef MyType = Int;
	**/
	function testEmpty(_) {
		eq(true, hasToplevel(toplevel(1), "type", "MyType"));
	}

	/**
		class Main {
			static function make<T>():Array<T> return [];
			static function main() {
				make.<Arr{-1-}>();
			}
		}
	**/
	function testPartial(_) {
		eq(true, hasToplevel(toplevel(1), "type", "Array"));
	}

	/**
		class Main {
			static function pair<A, B>(a:A, b:B):Void {}
			static function main() {
				pair.<Int, {-1-}
			}
		}
		typedef MyType = Int;
	**/
	function testSecond(_) {
		eq(true, hasToplevel(toplevel(1), "type", "MyType"));
	}
}
