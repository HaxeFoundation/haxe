package cases;

import Types;
using Lambda;

class Toplevel extends DisplayTestCase {
	/**
	class Main {
		static var myField;
		static function main() {{-1-}
			{-2-}
	**/
	function testToplevelResuming() {
		eq(true, hasToplevel(toplevel(pos(1)), "static", "myField"));
		eq(true, hasToplevel(toplevel(pos(2)), "static", "myField"));
	}

	/**
	class Main {
		static var myField;
		static function main() {
			{-1-}
			var a = "foo";
			{-2-}
	**/
	function testToplevelScoping() {
		var toplevel1 = toplevel(pos(1));
		var toplevel2 = toplevel(pos(2));
		eq(true, hasToplevel(toplevel1, "static", "myField"));
		eq(true, hasToplevel(toplevel2, "static", "myField"));
		eq(false, hasToplevel(toplevel1, "local", "a"));
		eq(true, hasToplevel(toplevel2, "local", "a"));
	}

	static function hasToplevel(a:Array<ToplevelElement>, kind:String, name:String):Bool {
		return a.exists(function(t) return t.kind == kind && t.name == name);
	}
}