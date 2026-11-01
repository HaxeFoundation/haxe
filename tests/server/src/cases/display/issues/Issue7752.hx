package cases.display.issues;

class Issue7752 extends DisplayTestCase {
	/**
		class Foo {
		extern function foo(te{-1-}st:Int):Void;
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("Int", result.item.type.args.path.typeName);
	}

	/**
		class Foo {
		extern function foo(test:Int = 1{-1-}2):Void;
		}
	**/
	function test2(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("Int", result.item.type.args.path.typeName);
	}
}
