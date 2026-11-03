package unit.teststd;

class TestEReg extends unit.Test {
	public function test() {
		var r = ~/a/;
		var rg = ~/a/g;
		var rg2 = ~/aa/g;
		f(r.match(""));
		f(r.match("b"));
		t(r.match("a"));
		eq(r.matched(0), "a");
		eq(r.matchedLeft(), "");
		eq(r.matchedRight(), "");
		var pos = r.matchedPos();
		eq(pos.pos, 0);
		eq(pos.len, 1);

		t(r.match("aa"));
		eq(r.matched(0), "a");
		eq(r.matchedLeft(), "");
		eq(r.matchedRight(), "a");
		var pos = r.matchedPos();
		eq(pos.pos, 0);
		eq(pos.len, 1);

		t(rg.match("aa"));
		eq(rg.matched(0), "a");
		eq(rg.matchedLeft(), "");
		eq(rg.matchedRight(), "a");
		var pos = rg.matchedPos();
		eq(pos.pos, 0);
		eq(pos.len, 1);

		t(rg2.match("aa"));
		eq(rg2.matched(0), "aa");
		eq(rg2.matchedLeft(), "");
		eq(rg2.matchedRight(), "");
		var pos = rg2.matchedPos();
		eq(pos.pos, 0);
		eq(pos.len, 2);

		t(rg2.match("AaaBaaC"));
		eq(rg2.matched(0), "aa");
		eq(rg2.matchedLeft(), "A");
		eq(rg2.matchedRight(), "BaaC");
		var pos = rg2.matchedPos();
		eq(pos.pos, 1);
		eq(pos.len, 2);

		// matched num
		var rg3 = ~/a/;
		var rg4 = ~/a(b)/;
		var rg5 = ~/a(b)(c)/;
		var rg6 = ~/a(b)(c)?/;

		t(rg3.match("a"));
		eq(rg3.matchedNum(), 1);
		f(rg3.match("b"));
		eq(rg3.matchedNum(), 0);

		f(rg4.match("a"));
		eq(rg4.matchedNum(), 0);
		t(rg4.match("ab"));
		eq(rg4.matchedNum(), 2);

		f(rg5.match("a"));
		eq(rg5.matchedNum(), 0);
		t(rg5.match("abc"));
		eq(rg5.matchedNum(), 3);
		t(rg6.match("ab"));
		eq(rg5.matchedNum(), 3);

		// split
		eq(~/a/.split("")[0], "");
		aeq(["", ""], ~/a/.split("a"));
		aeq(["", "a"], ~/a/.split("aa"));
		eq(~/a/.split("b")[0], "b");
		aeq(["", "b"], ~/a/.split("ab"));
		aeq(["b", ""], ~/a/.split("ba"));
		aeq(["", "ba"], ~/a/.split("aba"));
		aeq(["b", "b"], ~/a/.split("bab"));
		aeq(["b", "ba"], ~/a/.split("baba"));

		// split + g
		eq(~/a/g.split("")[0], "");
		aeq(["", ""], ~/a/g.split("a"));
		aeq(["", "", ""], ~/a/g.split("aa"));
		eq(~/a/g.split("b")[0], "b");
		aeq(["", "b"], ~/a/g.split("ab"));
		aeq(["b", ""], ~/a/g.split("ba"));
		aeq(["", "b", ""], ~/a/g.split("aba"));
		aeq(["b", "b"], ~/a/g.split("bab"));
		aeq(["b", "b", ""], ~/a/g.split("baba"));

		// replace
		eq(~/a/.replace("", "z"), "");
		eq(~/a/.replace("a", "z"), "z");
		eq(~/a/.replace("aa", "z"), "za");
		eq(~/a/.replace("b", "z"), "b");
		eq(~/a/.replace("ab", "z"), "zb");
		eq(~/a/.replace("ba", "z"), "bz");
		eq(~/a/.replace("aba", "z"), "zba");
		eq(~/a/.replace("bab", "z"), "bzb");
		eq(~/a/.replace("baba", "z"), "bzba");

		// replace + g
		eq(~/a/g.replace("", "z"), "");
		eq(~/a/g.replace("a", "z"), "z");
		eq(~/a/g.replace("aa", "z"), "zz");
		eq(~/a/g.replace("b", "z"), "b");
		eq(~/a/g.replace("ab", "z"), "zb");
		eq(~/a/g.replace("ba", "z"), "bz");
		eq(~/a/g.replace("aba", "z"), "zbz");
		eq(~/a/g.replace("bab", "z"), "bzb");
		eq(~/a/g.replace("baba", "z"), "bzbz");

		#if !(hl && interp) // not allowed in local hl interpreter, still allowed in hl runtime
		// replace + $
		eq(~/href="(.*?)"/.replace('lead href="foo" trail',"$1"), "lead foo trail");
		//~/href="(.*?)"/.replace('lead href="foo" trail',"$2") == "lead $2 trail";
		eq(~/href="(.*?)"/.replace('href="foo"',"$1"), "foo");
		//~/href="(.*?)"/.replace('href="foo"',"$2") == "$2";
		eq(~/href="(.*?)"/g.replace('lead href="foo" href="bar" trail',"$1"), "lead foo bar trail");
		eq(~/href="(.*?)"/g.replace('lead href="foo" href="bar" trail',"$$$1$$"), "lead $foo$ $bar$ trail");
		//~/href="(.*?)"/g.replace('lead href="foo" href="bar" trail',"$$$2$$") == "lead $$2$ $$2$ trail";
		#end

		eq(~/a(b)c/g.replace("abcabc", "$1"), "bb");
		eq(~/(a)|(b)/.replace("abc", '*'), "*bc");
		eq(~/(a)|(b)/g.replace("abc", '*'), "**c");

		// map
		eq(~/(Hello)/.map("Hello World", function(e) return "Hallo"), "Hallo World");
		eq(~/(Hello)/.map("Hello", function(e) return "Hallo"), "Hallo");
		eq(~/(World)/.map("Hello World", function(e) return "Hallo"), "Hello Hallo");
		eq(~/(Hola)/.map("Hello World", function(e) return throw "not called"), "Hello World");

		// escape
		t(new EReg("^" + EReg.escape("\\ ^ $ * + ? . ( ) | { } [ ]") + "$", "").match("\\ ^ $ * + ? . ( ) | { } [ ]"));

		// #6641
		aeq(["a", "c"], ~/(b)/.split("abc"));

		// #3430
		eq(~/(\d+)/g.replace("a1234b12","$1"), "a1234b12");
		eq(~/(\d+)/g.replace("a1234b12","\\$1"), "a\\1234b\\12");
		eq(~/(\d+)/g.replace("a1234b12","$$1"), "a$1b$1");

		// #10592 - null character
		#if !hl
		#if php
		// There is a bug in php < 8.2, see #10592
		if (php.Global.version_compare(php.Global.phpversion(), "8.2", ">=")) {
		#end
		var containingNull = new EReg("abc\x00def", "");
		f(containingNull.match("abc"));
		t(containingNull.match("abc\x00def"));
		f(containingNull.match("abc\x00fed"));
		var containingNull = ~/abc\x00def/;
		f(containingNull.match("abc"));
		t(containingNull.match("abc\x00def"));
		f(containingNull.match("abc\x00fed"));
		#if php
		}
		#end
		#end

	}
}
