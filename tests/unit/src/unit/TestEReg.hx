package unit;

class TestEReg extends Test {


	function test() {
		var r = ~/a+(b)?(c*)a+/;
		f( r.match("") );
		f( r.match("xxyy") );
		t( r.match("xxaabcayyy") );
		eq( r.matched(0), "aabca" );
		eq( r.matched(1), "b" );
		eq( r.matched(2), "c" );
		eq( r.matchedLeft(), "xx" );
		eq( r.matchedRight(), "yyy" );
		eq( r.matchedPos().pos, 2 );
		eq( r.matchedPos().len, 5 );

		t( r.match("aaa") );
		eq( r.matched(0), "aaa" );
		eq( r.matchedLeft(), "" );
		eq( r.matchedRight(), "" );
		t(r.matched(1) == null #if js || (js.Browser.supported && js.Browser.navigator.userAgent.indexOf('MSIE 8') > -1) #end); // JS/IE7-8 bug
		eq( r.matched(2), "" );
		unspec(function() r.matched(3));
		unspec(function() r.matched(-1));

		var r = ~/^(b)?$/;
		t( r.match("") );
		eq( r.matched(0), "" );
		t(r.matched(1) == null #if js || (js.Browser.supported && js.Browser.navigator.userAgent.indexOf('MSIE 8') > -1) #end); // JS/IE7-8 bug

		t( ~/\//.match("/") );

		t( ~/\n/.match("\n") );
		f( ~/\\n/.match("\n") );
		t( ~/\\n/.match("\\n") );

		t( ~/"/.match('"') );
		f( ~/\\"/.match('"') );
		t( ~/\\"/.match('\\"') );

		t( ~/\$/.match('$') );
		f( ~/\\$/.match('$') );
		f( ~/\\$/.match('\\$') );
		t( ~/\\\$/.match('\\$') );

		// check that global flag does not prevent matching several times (lastIndex in JS/Flash)
		var r = ~/cat/g;
		t( r.match("catneko") );
		t( r.match("catneko") );

		eq( ~/a+/.replace("aabbccaa", "x"), "xbbccaa" );
		eq( ~/a+/g.replace("aabbccaa", "x"), "xbbccx" );

		//testing split
		var test:String = "{ test } .blah  { something:someval } ";
		var block:EReg = ~/\s*\{\s*|\s*\}\s*/gm;
		eq( block.split(test).length, 5 );
		eq( '"' + block.split(test).join('","') + '"', '"","test",".blah","something:someval",""' );

		// test custom replace
		eq( ~/a+/g.map("aaabacx", function(r) return "[" + r.matched(0).substr(1) + "]") , "[aa]b[]cx" );
		eq( ~/a+/.map("aaabacx", function(r) return "[" + r.matched(0).substr(1) + "]") , "[aa]bacx" ); // same without 'g'

		eq( ~/a+(b*)/g.map("aaabacx", function(r) return "[" + r.matched(1) + "]") , "[b][]cx" );
		eq( ~/a+/g.map("aaabacx", function(r) return "[" + r.matchedRight() + "]") , "[bacx]b[cx]cx" );

		// we need to change our default customReplace implementation to fix that case
		// the best is to add a matchSub(s,pos,len)
		eq( ~/a+/g.map("aaabacxa", function(r) return "[" + r.matchedLeft() + "]") , "[]b[aaab]cx[aaabacx]" );

		// subsequent tests
		var r = ~/a+/g;
		eq(r.map("aaabacx", function(r) return "[" + r.matchedLeft() + "]") , "[]b[aaab]cx");
		eq(r.map("aaabacx", function(r) return "[" + r.matchedLeft() + "]") , "[]b[aaab]cx");

		// matchSub
		var r = ~/a+/;
		t(r.matchSub("abab", 0));
		eq(r.matchedRight(), "bab");
		t(r.matchSub("abab", 1));
		eq(r.matchedRight(), "b");
		eq(r.matchedLeft(), "ab");
		// same again to make sure state is correct
		t(r.matchSub("abab", 0));
		eq(r.matchedRight(), "bab");
		t(r.matchSub("abab", 1));
		eq(r.matchedRight(), "b");
		eq(r.matchedLeft(), "ab");
		// length
		f(r.matchSub("bbaa", 0, 1));
		f(r.matchSub("bbaa", 0, 2));
		f(r.matchSub("bbaa", 1, 1));
		t(r.matchSub("bbaa", 2, 1));
		eq(r.matchedLeft(), "bb");
		eq(r.matchedRight(), "a");

		eq( ~/x?/g.map("aaabacx", function(r) return "[" + r.matched(0)+ "]") , "[]a[]a[]a[]b[]a[]c[x]" );

		var f = function(x) return "([" +x.matchedLeft() + "]" + "[" +x.matched(0) + "]" + "[" +x.matchedRight() + "])b";
		var r = ~/$/mg;
		eq(r.map("\n", f), "([][][\n])b\n");
		eq(r.map("a", f), "a([a][][])b");
		eq(r.map("aa\na", f), "aa([aa][][\na])b\na([aa\na][][])b");
		//eq(r.map("", f, "")); // let's ignore this case

		var r = ~/^/mg;
		eq(r.map("\n", f), "([][][\n])b\n");
		eq(r.map("a", f), "([][][a])ba");
		eq(r.map("aa\na", f), "([][][aa\na])baa\n([aa\n][][a])ba");

		var r = ~/$/m;
		eq(r.map("\n", f), "([][][\n])b\n");
		eq(r.map("a", f), "a([a][][])b");
		eq(r.map("aa\na", f), "aa([aa][][\na])b\na");
		//eq(r.map("", f, "")); // let's ignore this case

		var r = ~/^/m;
		eq(r.map("\n", f), "([][][\n])b\n");
		eq(r.map("a", f), "([][][a])ba");
		eq(r.map("aa\na", f), "([][][aa\na])baa\na");

		var r = ~/a/;
		exc(function() r.matched(0));
	}

}