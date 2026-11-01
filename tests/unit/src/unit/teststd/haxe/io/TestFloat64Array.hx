package unit.teststd.haxe.io;

class TestFloat64Array extends unit.Test {
	public function test() {
		#if js
		if( untyped js.lib.Float64Array == "notsupported" ) return;
		#end

		var b = new haxe.io.Float64Array(5);
		eq(b[0], 0);
		eq(b[4], 0);
		eq(b.length, 5);

		// check float write
		b[1] = 1.25;
		feq(b[1], 1.25);

		// set
		for( i in 0...5 )
			b[i] = i + 1;
		eq(b[0], 1);
		eq(b[4], 5);

		// access outside bounds is unspecified but should not crash
		try b[-1] catch( e : Dynamic ) {};
		try b[5] catch(e : Dynamic) {};

		// same for writing
		try b[-1] = 55 catch( e : Dynamic ) {};
		try b[5] = 55 catch(e : Dynamic) {};

		var b2 = b.sub(1,3);
		eq(b2[0], 2);
		eq(b2[2], 4);
		eq(b2.length, 3);

		// check memory sharing
		b2[0] = 0xCC;
		eq(b2[0], 0xCC);
		eq(b[1], 0xCC);

		// should we allow writing past bounds ?
		try b2[-1] = 0xBB catch( e : Dynamic ) {};
		eq(b[0], 1);

		try b2[3] = 0xBB catch( e : Dynamic ) {};
		eq(b[4], 5);


		b.view == b.view; // no alloc

		eq(b.view.buffer, b2.view.buffer);
		eq(b.view.byteLength, 40);
		eq(b.view.byteOffset, 0);
		eq(b2.view.byteLength, 24);
		eq(b2.view.byteOffset, 8);

		// check sub
		var sub = b.sub(1);
		eq(sub.length, b.length - 1);
		eq(sub[0], 0xCC);
		sub[0] = 0xDD;
		eq(b[1], 0xDD);

		var sub = b.subarray(2,3);
		eq(sub.length, 1);
		eq(sub[0], 3);
		sub[0] = 0xEE;
		eq(b[2], 0xEE);

		// from bytes
		var b3 = haxe.io.Float64Array.fromBytes(b.view.buffer, 2*8, 3);
		eq(b3.length, 3);
		for( i in 0...3 )
			eq(b3[i], b[i+2]);
		b3[0] = b[3] + 1;
		eq(b3[0], b[2]);

	}
}
