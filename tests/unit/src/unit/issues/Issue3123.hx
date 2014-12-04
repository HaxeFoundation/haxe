package unit.issues;
using Lambda;

class Issue3123 extends Test
{
	function test() {
		var a = [1, 2, 3];
		var b = [1, 2];

		var length = a.filter(function(v) { return b.exists(function(v2) { return v == v2; } ); } );
		eq(2,length.count());
	}
}
