package unit.issues;
import unit.Test;

/** UInt not showing proper unsigned behavior */
class Issue2736 extends Test {
	function test() {
        var a:UInt, b:UInt;

        b = 50000;
        a = b * b;

        eq( a, b*b );
        eq( a/b, 50000 );
        eq(a % b, 0);

        f(a == b); t(a != b);
        t(a > b); t(a >= b);
        f(a < b); f(a <= b);

        // UInt vs Float comparisons
        f( a == 1.0 ); t( b == 50000.0 );
        t( a > 1.0 ); t( a >= 1.0 );
        f( a < -1.0 ); f( a <= 1.0 );

		#if false // ?
		var u:UInt = 2147483648;
        eq(1073741824, u >> 1);
        eq(1073741824, u >>> 1);
		#end
	}
}