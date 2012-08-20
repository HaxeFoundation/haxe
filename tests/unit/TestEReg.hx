package unit;

class TestEReg extends Test {


	function test() {
		#if !flash8
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
		eq( r.matched(1), null ); // JS/IE7-8 bug
		eq( r.matched(2), "" );
		unspec(function() r.matched(3));
		unspec(function() r.matched(-1));

		var r = ~/^(b)?$/;
		t( r.match("") );
		eq( r.matched(0), "" );
		eq( r.matched(1), null ); // JS/IE7-8 bug
		
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
		eq( ~/a+/g.customReplace("aaabacx", function(r) return "[" + r.matched(0).substr(1) + "]") , "[aa]b[]cx" );
		eq( ~/a+/.customReplace("aaabacx", function(r) return "[" + r.matched(0).substr(1) + "]") , "[aa]b[]cx" ); // same without 'g'
		
		eq( ~/a+(b*)/g.customReplace("aaabacx", function(r) return "[" + r.matched(1) + "]") , "[b][]cx" );
		eq( ~/a+/g.customReplace("aaabacx", function(r) return "[" + r.matchedRight() + "]") , "[bacx]b[cx]cx" );
		
		// we need to change our default customReplace implementation to fix that case
		// the best is to add a matchSub(s,pos,len)
		eq( ~/a+/g.customReplace("aaabacx", function(r) return "[" + r.matchedLeft() + "]") , "[]b[aaab]cx" );
		
		// this one creates infinite loops on too most of the platforms ! TOFIX !
		// eq( ~/x?/g.customReplace("aaabacx", function(r) return "[" + r.matched(0)+ "]") , "[]a[]a[]a[]b[]a[]c[][x]" );
		
		
		#end
	}

}