package unit;

class TestSerialize extends Test {

	function id<T>( v : T ) : T {
		return haxe.Unserializer.run(haxe.Serializer.run(v));
	}

	function test() {
		// basic types
		var values : Array<Dynamic> = [null, true, false, 0, 1, 1506, -0xABCDEF, 12.3, -1e10, "hello", "éé", "\r\n", "\n", "   ", ""];
		for( v in values )
			eq( id(v), v );

		t( Math.isNaN(id(Math.NaN)) );
		t( id(Math.POSITIVE_INFINITY) > 0 );
		f( id(Math.NEGATIVE_INFINITY) > 0 );
		f( Math.isFinite(id(Math.POSITIVE_INFINITY)) );
		f( Math.isFinite(id(Math.NEGATIVE_INFINITY)) );

		// array/list
		doTestCollection([]);
		doTestCollection([1,2,4,5]);
		doTestCollection([1,2,null,null,null,null,null,4,5]);

		// date
		var d = Date.now();
		var d2 = id(d);
		t( Std.is(d2,Date) );
		eq( d2.toString(), d.toString() );

		// object
		var o = { x : "a", y : -1.56, z : "hello" };
		var o2 = id(o);
		eq(o.x,o2.x);
		eq(o.y,o2.y);
		eq(o.z,o2.z);

		// class instance
		var c = new MyClass(999);
		c.intValue = 33;
		c.stringValue = "Hello";
		var c2 = id(c);
		t( Std.is(c2,MyClass) );
		f( c == c2 );
		eq( c2.intValue, c.intValue );
		eq( c2.stringValue, c.stringValue );
		eq( c2.get(), 999 );

		// enums
		haxe.Serializer.USE_ENUM_INDEX = false;
		doTestEnums();
		haxe.Serializer.USE_ENUM_INDEX = true;
		doTestEnums();

		// StringMap
		var h = new haxe.ds.StringMap();
		h.set("keya",2);
		h.set("kéyb",-465);
		var h2 = id(h);
		t( Std.is(h2,haxe.ds.StringMap) );
		eq( h2.get("keya"), 2 );
		eq( h2.get("kéyb"), -465 );
		eq( Lambda.count(h2), 2 );

		// IntMap
		var h = new haxe.ds.IntMap();
		h.set(55,2);
		h.set(-101,-465);
		var h2 = id(h);
		t( Std.is(h2,haxe.ds.IntMap) );
		eq( h2.get(55), 2 );
		eq( h2.get(-101), -465 );
		eq( Lambda.count(h2), 2 );

		// ObjectMap
		var h = new haxe.ds.ObjectMap();
		var a = new unit.MyAbstract.ClassWithoutHashCode(9);
		var b = new unit.MyAbstract.ClassWithoutHashCode(8);
		h.set(a, b);
		h.set(b, a);
		var h2 = id(h);
		t(Std.is(h2, haxe.ds.ObjectMap));
		// these are NOT the same objects
		f(h2.exists(a));
		f(h2.exists(b));
		// all these should still work
		t(h.exists(a));
		t(h.exists(b));
		eq(h.get(a), b);
		eq(h.get(b), a);
		var nothing = true;
		for (k in h2.keys()) {
			nothing = false;
			t(k.i == 8 || k.i == 9);
			t(h2.exists(k));
			var v = h2.get(k);
			t(v.i == 8 || v.i == 9);
		}
		f(nothing);
		
		// bytes
		doTestBytes(haxe.io.Bytes.alloc(0));
		doTestBytes(haxe.io.Bytes.ofString("A"));
		doTestBytes(haxe.io.Bytes.ofString("AB"));
		doTestBytes(haxe.io.Bytes.ofString("ABC"));
		doTestBytes(haxe.io.Bytes.ofString("ABCD"));
		doTestBytes(haxe.io.Bytes.ofString("héllé"));
		var b = haxe.io.Bytes.alloc(100);
		for( i in 0...b.length )
			b.set(i,i%10);
		doTestBytes(b);

		// recursivity
		c.ref = c;
		haxe.Serializer.USE_CACHE = true;
		var c2 = id(c);
		haxe.Serializer.USE_CACHE = false;
		eq( c2.ref, c2 );

		// errors
		#if !cpp
		exc(function() haxe.Unserializer.run(null));
		#end

		exc(function() haxe.Unserializer.run(""));

	}

	function doTestEnums() {
		eq( id(MyEnum.A), MyEnum.A );
		eq( id(MyEnum.B), MyEnum.B );
		var c = MyEnum.C(0,"hello");
		t( Type.enumEq( id(c), c ) );
		t( Type.enumEq( id(MyEnum.D(MyEnum.D(c))), MyEnum.D(MyEnum.D(c)) ) );
		t( Std.is(id(c),MyEnum) );
		t(switch( id(c) ) {
			case C(_,_): true;
			default: false;
		});
	}

	function doTestCollection( a : Array<Dynamic> ) {
		var a2 = id(a);
		eq( a2.length, a.length );
		for( i in 0...a.length )
			eq( a2[i], a[i] );
		var l = Lambda.list(a);
		var l2 = id(l);
		t( Std.is(l2,List) );
		eq( l2.length, l.length );
		var it = l.iterator();
		for( x in l2 )
			eq( x, it.next() );
		f( it.hasNext() );
	}

	function doTestBytes( b : haxe.io.Bytes ) {
		var b2 = id(b);
		t( Std.is(b2,haxe.io.Bytes) );
		eq( b2.length, b.length );
		for( i in 0...b.length )
			eq( b2.get(i), b.get(i) );
		infos(null);
	}

}