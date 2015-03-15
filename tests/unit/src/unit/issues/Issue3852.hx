package unit.issues;

class Issue3852 extends Test {
    @:analyzer(no_const_propagation)
    function test() {
        var u:UInt = 4;
        var i:Int = 3;
        var d:Float = 3;

        eq(u + i, 7);
        eq(u + d, 7);
        eq(i + u, 7);
        eq(d + u, 7);

        eq(u * i, 12);
        eq(u * d, 12);
        eq(i * u, 12);
        eq(d * u, 12);

        eq(u % i, 1);
        eq(u % d, 1);
        eq(i % u, 3);
        eq(d % u, 3);

        eq(u - i, 1);
        eq(u - d, 1);
        eq(i - u, (-1 : UInt));
        eq(d - u, -1);

        #if !flash // flash generator errors on these
        t(u > i);
        t(u > d);
        f(i > u);
        f(d > u);
        t(u >= i);
        t(u >= d);
        f(i >= u);
        f(d >= u);

        f(u < i);
        f(u < d);
        t(i < u);
        t(d < u);
        f(u <= i);
        f(u <= d);
        t(i <= u);
        t(d <= u);
        #end

        i = 5;
        d = 5;
        eq(u / i, 0.8);
        eq(u / d, 0.8);
        eq(i / u, 1.25);
        eq(d / u, 1.25);

        u = 8;
        i = 2;
        eq(u << i, 32);
        eq(i << u, 512);
        eq(u >> i, 2);
        eq(i >> u, 0);
    }
}
