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
		eq( r.matched(1), null );
		eq( r.matched(2), "" );
		exc(function() r.matched(3));
		#end
	}

}