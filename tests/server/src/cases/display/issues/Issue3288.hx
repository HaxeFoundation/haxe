package cases.display.issues;

class Issue3288 extends DisplayTestCase {
	/**
		class Main {
		    static function main() {
		        WithType.{-1-}
		    }
		}
	**/
	function testWithType(_) {
		vfs.putContent("WithType.hx", "
class WithType {
    public static var f1:Int;
    public static function f2():Void {}
}
class OtherType {}
typedef AnotherOne = Int
");
		var items = fields(1);
		Assert.isTrue(hasField(items, "f1"));
		Assert.isTrue(hasField(items, "f2"));
		Assert.isTrue(hasPath(items, "AnotherOne") || hasPath(items, "OtherType"));
	}

	/**
		class Main {
		    static function main() {
		        WithoutType.{-1-}
		    }
		}
	**/
	function testWithoutType(_) {
		vfs.putContent("WithoutType.hx", "
class SomeType {}
enum OtherType {}
");
		var items = fields(1);
		Assert.isTrue(hasPath(items, "OtherType") || hasPath(items, "SomeType"));
	}

	/**
		class Main {
		    static function main() {
		        WithEmptyType.{-1-}
		    }
		}
	**/
	function testWithEmptyType(_) {
		vfs.putContent("WithEmptyType.hx", "
class WithEmptyType {}
class OtherType {}
typedef AnotherOne = Int
");
		var items = fields(1);
		Assert.isTrue(hasPath(items, "AnotherOne") || hasPath(items, "OtherType"));
	}
}
