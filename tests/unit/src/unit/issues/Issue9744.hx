package unit.issues;

private abstract Ref<T>(haxe.ds.Vector<T>) {
	public var value(get, set):T;

	public inline function new() this = new haxe.ds.Vector(1);

	@:to inline function get_value():T return this[0];
	inline function set_value(param:T) return this[0] = param;

	public function toString():String return '@[' + Std.string(value)+']';

	@:noUsing @:from static inline public function to<A>(v:A):Ref<A> {
	  var ret = new Ref();
	  ret.value = v;
	  return ret;
	}
}

private interface IVcs {
	var available(get, set):Bool;
}

private class Vcs implements IVcs {
	@:isVar public var available(get, set):Bool;

	function get_available() {
		return available;
	}

	function set_available(v:Bool) {
		return available = v;
	}

	public function new(available:Bool) {
		this.available = available;
	}
}

#if eval
/* We don't care about the actual extern behavior so there's no point in trying to get this test to
   work on all targets. The only important part here is that the typing doesn't choke on the missing
   accessor, which will behave the same way on all targets. */

@:native("Issue9744NativeVirtualArray")
private class NativeVirtualArrayNonExtern {
	function new() {}
	var length(get, null):Int;

	function get_length() {
		return 1;
	}
}

@:native("Issue9744NativeVirtualArray")
private extern class NativeVirtualArray {
	function new():Void;
	var length(get, null):Int;
}

#end

class NadakoBase {
	function f() {}
}
class NadakoA extends NadakoBase {}

@:access(unit.issues.NadakoA)
class NadakoB {
	var a:NadakoA;
	function f() {
		a.f();
	}
}

class Issue9744 extends unit.Test {
	#if ((cs && fast_cast && erase_generics) || cppia)
	#else
	function testAbstractOverAbstractSelf() {
		var ref = new Ref();
		eq(1, ref.value = 1);
		eq(1, ref.value);
	}
	#end

	function testUnopProperties() {
		var vcs = new Vcs(true);
		t(vcs.available);
		f(!vcs.available);
		t(!(vcs.available = false));

		var ivcs:IVcs = new Vcs(true);
		t(ivcs.available);
		f(!ivcs.available);
		t(!(ivcs.available = false));
	}

	#if eval
	function testExternProperty() {
		var nva = new NativeVirtualArray();
		eq(1, nva.length);
	}
	#end

	function testUnopResolve() {
		var doc = Xml.parse('<node attr="1"/>');
		var access = new haxe.xml.Access(doc.firstElement());
		f(!access.has.attr);
		t(!access.has.attra);
	}
}