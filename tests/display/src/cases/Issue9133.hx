package cases;

import utest.Assert;

using Lambda;

class Issue9133 extends DisplayTestCase {
	/**
		class Main {
		static function main() {
			var i = 1;
			var s = "";

			var map:Map<Int, Int> = [
				{-1-}
	**/
	function test1() {
		var fields = toplevel(pos(1));
		var i1 = fields.findIndex(item -> item.kind == "local" && item.name == "i");
		var i2 = fields.findIndex(item -> item.kind == "local" && item.name == "s");
		Assert.isTrue(i1 < i2);
		Assert.isTrue(i1 != -1);
	}

	/**
		class Main {
		static function main() {
			var i = 1;
			var s = "";

			var map:Map<Int, Int> = [
				i => 1,
				{-1-}
	**/
	function test2() {
		// Note: One could argue if `i` should really be suggested here because it currently
		// causes a `Duplicate key` error. See https://github.com/HaxeFoundation/haxe/issues/9144
		// for more context.
		var fields = toplevel(pos(1));
		var i1 = fields.findIndex(item -> item.kind == "local" && item.name == "i");
		var i2 = fields.findIndex(item -> item.kind == "local" && item.name == "s");
		Assert.isTrue(i1 < i2);
		Assert.isTrue(i1 != -1);
	}

	/**
		class Main {
		static function main() {
			var i = 0;
			{-1-}// comment
	**/
	function test3() {
		var fields = toplevel(pos(1));
		var i1 = fields.findIndex(item -> item.kind == "local" && item.name == "i");
		Assert.isTrue(i1 != -1);
	}
}
