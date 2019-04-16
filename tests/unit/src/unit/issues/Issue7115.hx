package unit.issues;

private interface Interface { }

class Issue7115 extends unit.Test {
	function testIs() {
		f(Std.is(getNull(), Int));
		f(Std.is(getNull(), Float));
		f(Std.is(getNull(), Bool));
		f(Std.is(getNull(), String));
		f(Std.is(getNull(), Issue7115));
		f(Std.is(getNull(), haxe.ds.Option));
		f(Std.is(getNull(), Dynamic));
		f(Std.is(getNull(), null));
		f(Std.is(getNull(), Interface));
	}

	function testCast() {
		#if !flash // ???
		eq(#if static 0 #else null #end, cast(getNull(), Int));
		eq(#if static 0. #else null #end, cast(getNull(), Float));
		eq(#if static false #else null #end, cast(getNull(), Bool));
		eq(null, cast(getNull(), String));
		#end
		eq(null, cast(getNull(), Issue7115));
		eq(null, cast(getNull(), haxe.ds.Option<Dynamic>));
		eq(null, cast(getNull(), Interface));
	}

	function testCatch() {
		var expected = #if static 1 #else 2 #end;

		var i = try {
			throw (getNull() : Int);
		} catch(s:Int) {
			1;
		} catch(e:Dynamic) {
			2;
		}
		eq(expected, i);

		var i = try {
			throw (getNull() : Float);
		} catch(s:Float) {
			1;
		} catch(e:Dynamic) {
			2;
		}
		eq(expected, i);

		var i = try {
			throw (getNull() : Bool);
		} catch(s:Bool) {
			1;
		} catch(e:Dynamic) {
			2;
		}
		eq(expected, i);

		var i = try {
			throw (getNull() : String);
		} catch(s:String) {
			1;
		} catch(e:Dynamic) {
			2;
		}
		eq(2, i);

		var i = try {
			throw (getNull() : Issue7115);
		} catch(s:Issue7115) {
			1;
		} catch(e:Dynamic) {
			2;
		}
		eq(2, i);

		var i = try {
			throw (getNull() : haxe.ds.Option<Dynamic>);
		} catch(s:haxe.ds.Option<Dynamic>) {
			1;
		} catch(e:Dynamic) {
			2;
		}
		eq(2, i);

		var i = try {
			throw (getNull() : Interface);
		} catch(s:Interface) {
			1;
		} catch(e:Dynamic) {
			2;
		}
		eq(2, i);
	}

	static function getNull<T>():T {
		return null;
	}
}
