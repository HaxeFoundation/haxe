package cases;

class Issue8007 extends DisplayTestCase {
	/**
		var i:Null<{-1-}>
	**/
	@:funcCode
	function test1() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		var i:Null<{-1-}
	**/
	@:funcCode
	function test2() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}
}
