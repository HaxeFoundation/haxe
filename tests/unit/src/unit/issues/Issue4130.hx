package unit.issues;

import haxe.Int64;

private class TestKey
{
   public function new() { }
}

class Issue4130 extends Test
{
   #if cpp
	public function test()
	{
        var a = [1];

        var val64 = Int64.make(42,24);
        var valPtr = cpp.Pointer.arrayElem(a,0);

        var int2Int64 = new haxe.ds.IntMap<Int64>();
        int2Int64.set( 42, val64);
        var key:Dynamic = 43;
        int2Int64.set( key, val64);
        eq(int2Int64.get(42),val64);
        eq(int2Int64.get(43),val64);


        var int2Ptr = new haxe.ds.IntMap< cpp.Pointer<Int> >();
        int2Ptr.set( 42, valPtr );
        int2Ptr.set( key, valPtr );
        eq( int2Ptr.get(42), valPtr);
        eq( int2Ptr.get(key), valPtr);


        var string2Int64 = new haxe.ds.StringMap<Int64>();
        string2Int64.set( "42", val64 );
        var key:Dynamic = "43";
        string2Int64.set( "43", val64 );
        eq( string2Int64.get("42"), val64 );
        eq( string2Int64.get(key), val64 );

        var string2Ptr = new haxe.ds.StringMap< cpp.Pointer<Int> >();
        string2Ptr.set( "42", valPtr );
        string2Ptr.set( key, valPtr );
        eq( string2Ptr.get("42"), valPtr );
        eq( string2Ptr.get(key), valPtr );


        var key = new TestKey();
        var obj2Int64 = new haxe.ds.ObjectMap<TestKey,Int64>();
        obj2Int64.set( key, val64);
        eq( obj2Int64.get(key), val64 );

        var obj2Ptr = new haxe.ds.ObjectMap<TestKey, cpp.Pointer<Int> >();
        obj2Ptr.set( key, valPtr );
        eq( obj2Ptr.get(key), valPtr );


        var weak2Int64 = new haxe.ds.WeakMap<TestKey,Int64>();
        weak2Int64.set( key, val64);
        eq( weak2Int64.get(key),val64 );

        var weak2Ptr = new haxe.ds.WeakMap<TestKey, cpp.Pointer<Int> >();
        weak2Ptr.set( key, valPtr );
        eq( weak2Ptr.get(key), valPtr );

	}
   #end
}


