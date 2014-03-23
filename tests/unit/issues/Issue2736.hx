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

        /* These are currently broken but should be fixed in the future:
		 * Currently we don't allow UInt vs Int comparisons.
         * trace(a == -1794967296);

         * Shift on swf9 return as Int and not UInt
         * trace(a >> 1); //-897483648 in flash, but 3397483648 in neko and js
         */
	}
}