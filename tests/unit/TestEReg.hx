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
		eq( r.matched(1), null ); // JS/IE7 bug
		eq( r.matched(2), "" );
		unspec(function() r.matched(3));
		unspec(function() r.matched(-1));

		var r = ~/^(b)?$/;
		t( r.match("") );
		eq( r.matched(0), "" );
		eq( r.matched(1), null ); // JS/IE7 bug
		
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
		
		#end
	}

}