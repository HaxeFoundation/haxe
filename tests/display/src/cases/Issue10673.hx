package cases;

import haxe.display.Display;
import haxe.display.FsPath;

class Issue10673 extends DisplayTestCase {
	#if (display.protocol == "jsonrpc")
	/**
		@:build(issue10673.Macro.build())
		class Main {
			var a:Int;

			function func1():Void {
				var lhs_______ = 0;
				var {-10-}rhs_______{-11-} = 0;
				lhs_______ = rhs_____{-1-}__;
				lhs_______ = rhs_____{-2-}__;
				lhs_______ = rhs_____{-3-}__;
				lhs_______ = rhs_____{-4-}__;
				lhs_______ = rhs_____{-5-}__;
				lhs_______ = rhs_____{-6-}__;
				lhs_______ = rhs_____{-7-}__;
				lhs_______ = rhs_____{-8-}__;
				lhs_______ = rhs_____{-9-}__;
			}
		}
	**/
	function test() {
		var defRange = range(10, 11);

		function hover(marker:Int) @:privateAccess {
			var ret = ctx.callDisplay(DisplayMethods.Hover, {
				file: new FsPath(ctx.source.path),
				offset: pos(marker),
			}).result;

			switch (ret.item.kind) {
				case Local:
					eq("rhs_______", ret.item.args.name);
					var pos = ret.item.args.pos;
					eq(ctx.source.path, pos.file);
					var range = ctx.source.formatRange(pos.min, pos.max);
					eq(defRange, ctx.normalizePath(range));

				case _:
					utest.Assert.fail("Wrong hover item kind");
			}
		}

		for (i in 1...10) hover(i);
	}
	#end
}
