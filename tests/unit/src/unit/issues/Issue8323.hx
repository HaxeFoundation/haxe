package unit.issues;

@:autoBuild(foo.macro.MyMacros.build())
interface Issue8323Foo {}

@:autoBuild(foo.macro.MyMacros.build())
interface Issue8323Bar {}

@:autoBuild(foo.macro.MyMacros.build())
interface Issue8323FooBar {}

class Issue8323 extends Test {
	function test() {
		eq("foo.macro.MyMacros.build()", getAutoBuild("Issue8323Foo"));
		eq("foo.macro.MyMacros.build()", getAutoBuild("Issue8323Bar"));
		eq("foo.macro.MyMacros.build()", getAutoBuild("Issue8323FooBar"));
	}

	static macro function getAutoBuild(name:String) {
		var cl = haxe.macro.TypeTools.getClass(haxe.macro.Context.getType("unit.issues.Issue8323." + name));
		var meta = cl.meta.extract(":autoBuild")[0];
		return macro $v{haxe.macro.ExprTools.toString(meta.params[0])};
	}
}
