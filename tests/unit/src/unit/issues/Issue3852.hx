package unit.issues;

class Issue3852 extends Test {
	@:analyzer(no_const_propagation)
	function test() {
		var u:UInt = 4;
		var i:Int = 3;
		var d:Float = 3;

		eq(u + i, 7);
		#if loose_numeric_casts
		eq(u + d, 7);
		#end
		eq(i + u, 7);
		#if loose_numeric_casts
		eq(d + u, 7);
		#end

		eq(u * i, 12);
		#if loose_numeric_casts
		eq(u * d, 12);
		#end
		eq(i * u, 12);
		#if loose_numeric_casts
		eq(d * u, 12);
		#end

		eq(u % i, 1);
		#if loose_numeric_casts
		eq(u % d, 1);
		#end
		eq(i % u, 3);
		#if loose_numeric_casts
		eq(d % u, 3);
		#end

		eq(u - i, 1);
		#if loose_numeric_casts
		eq(u - d, 1);
		#end
		eq(i - u, (-1 : UInt));
		#if loose_numeric_casts
		eq(d - u, -1);
		#end

		#if !flash // flash generator errors on these
		t(u > i);
		#if loose_numeric_casts
		t(u > d);
		#end
		f(i > u);
		#if loose_numeric_casts
		f(d > u);
		#end
		t(u >= i);
		#if loose_numeric_casts
		t(u >= d);
		#end
		f(i >= u);
		#if loose_numeric_casts
		f(d >= u);
		#end

		f(u < i);
		#if loose_numeric_casts
		f(u < d);
		#end
		t(i < u);
		#if loose_numeric_casts
		t(d < u);
		#end
		f(u <= i);
		#if loose_numeric_casts
		f(u <= d);
		#end
		t(i <= u);
		#if loose_numeric_casts
		t(d <= u);
		#end
		#end

		i = 5;
		d = 5;
		#if loose_numeric_casts
		eq(u / d, 0.8);
		eq(d / u, 1.25);
		#end

		u = 8;
		i = 2;
		eq(u << i, 32);
		eq(u >> i, 2);
	}
}
