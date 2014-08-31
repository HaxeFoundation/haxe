package unit;
using haxe.Int64;
import haxe.Int64.*;

class TestInt64 extends Test {

	public function test() {
		eq( 1.ofInt().toInt(), 1 );
		eq( ( -1).ofInt().toInt(), -1 );
		eq( Std.string(156.ofInt()), "156" );

		var v = (1 << 20).ofInt();
		eq( Std.string(v), "1048576" );

		var p40 = v.shl(20);
		eq( p40.getLow(), 0 );
		eq( p40.getHigh(), 256 );
		eq( Std.string(p40), "1099511627776" );

		eq( 1.ofInt().shl(0).toStr(), "1" );

		eq(Int64.ofInt(0).toStr(), "0");
	}

	public function testMath() {
		var a = Int64.make(0, 0x239B0E13);
		var b = Int64.make(0, 0x39193D1B);
		var c = Int64.mul(a, b);
		eq( c.toStr(), "572248275467371265" );
		// trace(Int64.toStr(c) + " should be 572248275467371265"); // but gives 7572248271172403969 in javascript

		var a = Int64.make(0, 0xD3F9C9F4);
		var b = Int64.make(0, 0xC865C765);
		var c = Int64.mul(a, b);
		eq( c.toStr(), "-6489849317865727676" );

		var a = Int64.make(0, 0x9E370301);
		var b = Int64.make(0, 0xB0590000);
		var c = Int64.add(a, b);
		eq( Int64.toStr(c), "5613028097" );

		var a = Int64.make(0xFFF21CDA, 0x972E8BA3);
		var b = Int64.make(0x0098C29B, 0x81000001);
		var c = Int64.mul(a, b);
		#if !as3
		var expected = Int64.make(0xDDE8A2E8, 0xBA2E8BA3);
		eq( expected.compare(c), 0 );
		#end
	}

	// tests ported from https://github.com/candu/node-int64-native/blob/master/test/int64.js
	public function testCompare()
	{
    var a = ofInt(2),
        b = ofInt(3);
		t(a == a);
		t(b == b);
		eq(a.compare(a), 0);
		eq(a.compare(b), -1);
		eq(b.compare(a), 1);
	}

	public function testBits()
	{
	  var x = make(0xfedcba98,0x76543210);
    var y = x.and((ofInt(0xffff))),
        z = x.or((ofInt(0xffff))),
        w = x.xor((make(0xffffffff,0xffffffff)));
    eq(y.toStr(), '12816');
    eq(z.toStr(), '-81985529216434177');
    eq(w.toStr(), '81985529216486895');
    eq(x.and(ofInt(0xffff)).toStr(), '12816');
    eq((x.or(ofInt(0xffff))).toStr(), '-81985529216434177');
    eq((x.xor(ofInt(0xffff))).toStr(), '-81985529216446993');
    eq((x.and(make(0x1,0xffffffff))).toStr(), '1985229328');
    eq((x.or(make(0x1,0xffffffff))).toStr(), '-81985522611781633');
    eq((x.xor(make(0x1, 0xffffffff))).toStr(), '-81985524597010961');
    var a = ofInt(7),
        b = a.shl(1);
    eq(b.toStr(), '14');
	}

	public function testAdd()
	{
		var a = ofInt(3),
				b = ofInt(2),
				c = make(0xffffffff,0xfffffffe);
		eq( (a.add(b)).compare(ofInt(5)), 0 );
		eq( (a.add(ofInt(4))).compare(ofInt(7)), 0 );
		eq( (c.add(ofInt(3))).compare(ofInt(1)), 0 );
		// numbers larger than int32
		eq( a.add(make(0x1, 0)).toStr(), '4294967299');
	}

}
