package cases.display.issues;

class Issue11018 extends DisplayTestCase {
	/**
		typedef Foo = {
			?someField:String,
			?secondField:Int,
		}

		class Main {
			static function main() {
				final foo:Foo = {
					s{-1-}
					secondField: 10,
				}
				final fooSingle:Foo = {
					s{-2-}
				}
			}
		}
	**/
	function test(_) {
		final result1 = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: true});
		Assert.equals("s", result1.filterString);
		eq(true, hasField(result1.items, "someField", "Null<String>"));
		eq(false, hasField(result1.items, "secondField", "Null<Int>"));

		final result2 = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: true});
		Assert.equals("s", result2.filterString);
		eq(true, hasField(result2.items, "someField", "Null<String>"));
		eq(true, hasField(result2.items, "secondField", "Null<Int>"));
	}

	/**
		typedef Foo = {
			?firstField:Bool,
			?someField:String,
			?secondField:Int,
		}

		class Main {
			static function main() {
				final foo1:Foo = {
					s{-1-}
					secondField: 10,
					firstField: true,
				}
				final foo2:Foo = {
					firstField: true,
					s{-2-}
					secondField: 10,
				}
			}
		}
	**/
	function testMultipleFields(_) {
		for (i in 1...3) {
			final result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(i), wasAutoTriggered: true});
			Assert.equals("s", result.filterString);
			eq(true, hasField(result.items, "someField", "Null<String>"));
			eq(false, hasField(result.items, "secondField", "Null<Int>"));
			eq(false, hasField(result.items, "firstField", "Null<Bool>"));
		}
	}
}
