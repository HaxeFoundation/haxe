package cases;

import haxe.display.Display;
import haxe.display.FsPath;

class Issue8712 extends DisplayTestCase {
	#if (display.protocol == "jsonrpc")
	/**
		abstract Foo(Int) {
			function foo() {
				ab{-2-}stract;
				ab{-3-}stract.bar();
			}

			function bar() return th{-1-}is;
		}
	**/
	function test() @:privateAccess {
		function hover(marker:Int, ?p:haxe.PosInfos) {
			var item = ctx.callDisplay(DisplayMethods.Hover, {
				file: new FsPath(ctx.source.path),
				offset: pos(marker),
			}).result.item;

			switch (item.kind) {
				case Local if (marker == 1):
					eq("this", item.args.name);
					eq(null, item.args.origin); // = NOT VUser

				case Literal if (marker >= 2):
					eq("abstract", item.args.name);

				case kind:
					utest.Assert.fail("Unexpected item kind: " + kind, p);
			}
		}

		for (i in 1...4) hover(i);
	}
	#end
}
