package unit;

@enumMeta private enum E {
	@a A;
	@b(0) B;
}

@classMeta("x") class TestMeta extends Test {

	@empty() @_int(-45) @complex([{ x : 0, y : "hello", z : -1.48, b : true, k : null }]) static var foo : Int;

	@new public function new() {
		super();
	}

	function fields( o : Dynamic ) {
		if( o == null ) return null;
		var fl = Reflect.fields(o);
		fl.sort(Reflect.compare);
		return fl.join("#");
	}

	public function testMeta() {
		var m = haxe.rtti.Meta.getType(E);
		eq( fields(m), "enumMeta" );
		eq( m.enumMeta, null );

		var m = haxe.rtti.Meta.getType(TestMeta);
		eq( fields(m), "classMeta" );
		eq( Std.string(m.classMeta), "[x]" );

		var m = haxe.rtti.Meta.getFields(E);
		eq( fields(m), "A#B" );
		eq( fields(m.A), "a" );
		eq( m.A.a, null );
		eq( fields(m.B), "b" );
		eq( Std.string(m.B.b), "[0]" );

		var m = haxe.rtti.Meta.getFields(TestMeta);
		eq( fields(m), "_" );
		eq( fields(m._), #if as3 "_"+#end "new" );

		var m = haxe.rtti.Meta.getStatics(TestMeta);
		eq( fields(m), "foo" );
		eq( fields(m.foo), "_int#complex#empty" );
		eq( m.foo.empty, null );
		eq( Std.string(m.foo._int), "[-45]" );
		var c : Dynamic = m.foo.complex[0][0];
		eq( fields(c), "b#k#x#y#z" );
		eq( c.x, 0 );
		eq( c.y, "hello" );
		eq( c.z, -1.48 );
		eq( c.b, true );
		eq( c.k, null );
	}

	public function testExprMeta() {
		eq(getMeta(@foo a).name, "foo");
		eq(getMeta(@foo("a") b).name, "foo");
		eq(getMeta(@foo ("a")).name, "foo");
		
		var m = getMeta(@bar("1", "foo") null);
		eq(m.name, "bar");
		eq(m.args[0], "1");
		eq(m.args[1], "foo");
		
		eq(getMeta(@foo ("1")).args.length, 0);
		eq(getMeta(@foo("1") "2").args.length, 1);
	}

	static macro function getMeta(e) {
		switch(e.expr) {
			case EMeta(m, _):
				return macro { name: $v{m.name}, args: $a{m.params} };
			default:
				return macro report("Metadata expected");
		}
	}
}