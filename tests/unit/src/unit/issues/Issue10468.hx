package unit.issues;

import haxe.SysTools;

class Issue10468 extends Test {
	function test() {
		eq('"some/path"', SysTools.quoteWinArg('some/path', false));
		eq('/S', SysTools.quoteWinArg('/S', false));
	}
}
