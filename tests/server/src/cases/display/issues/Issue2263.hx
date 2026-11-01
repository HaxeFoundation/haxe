package cases.display.issues;

class Issue2263 extends DisplayTestCase {
	static final myModuleHx = "
class MyModule {
    public static function pubM() {}
    public static var pubV = 10;
    static function privM() {}
    static var privV = 10;
}

class OtherType {
    public static function pubM() {}
    public static var pubV = 10;
}
";

	/**
		import MyModule.{-1-}
	**/
	function testImportCompletion(_) {
		vfs.putContent("MyModule.hx", myModuleHx);
		var items = fields(1);
		Assert.isTrue(hasField(items, "pubV"));
		Assert.isTrue(hasField(items, "pubM"));
		Assert.isTrue(hasPath(items, "MyModule"));
		Assert.isTrue(hasPath(items, "OtherType"));
	}

	/**
		class C {
		    function f() {
		        var v:MyModule.{-1-}
		    }
		}
	**/
	function testTypeCompletion(_) {
		vfs.putContent("MyModule.hx", myModuleHx);
		var items = fields(1);
		Assert.isTrue(hasPath(items, "MyModule"));
		Assert.isTrue(hasPath(items, "OtherType"));
	}

	/**
		import MyModule.OtherType.{-1-}
	**/
	function testSubtypeStaticCompletion(_) {
		vfs.putContent("MyModule.hx", myModuleHx);
		var items = fields(1);
		Assert.isTrue(hasField(items, "pubV"));
		Assert.isTrue(hasField(items, "pubM"));
	}
}
