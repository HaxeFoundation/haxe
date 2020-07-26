package unit.issues;

private class PropertyClass {
	public var propGet(get, default) = 0;
	public var propSet(default, set) = 0;
	@:isVar public var propGetSet(get, set) = 0;

	public var numGetterCalls:Int;
	public var numSetterCalls:Int;

	public function new() {
		numGetterCalls = 0;
		numSetterCalls = 0;
	}

	function get_propGetSet() {
		numGetterCalls++;
		return propGetSet;
	}

	function set_propSet(value:Int) {
		numSetterCalls++;
		return this.propSet = value;
	}

	function get_propGet() {
		numGetterCalls++;
		return propGet;
	}

	function set_propGetSet(value:Int) {
		numSetterCalls++;
		return propGetSet = value;
	}
}

private typedef PropertyAbstractState = {
	var propertyValue:Int;
	var numGetterCalls:Int;
	var numSetterCalls:Int;
}

private abstract PropertyAbstract(PropertyAbstractState) {
	public var propGetSet(get, set):Int;

	public function new() {
		this = {
			propertyValue: 0,
			numGetterCalls: 0,
			numSetterCalls: 0
		}
	}

	function get_propGetSet() {
		this.numGetterCalls++;
		return this.propertyValue;
	}

	function set_propGetSet(value:Int) {
		this.numSetterCalls++;
		return this.propertyValue = value;
	}

	public function getThis() {
		return this;
	}
}

private class PropertyClassTestContext extends unit.Test {
	public var index:Int;

	var numCalled:Int;

	var c:PropertyClass;

	public function new() {
		super();
		index = 0;
		numCalled = 0;
		c = new PropertyClass();
	}

	public function get() {
		numCalled++;
		return [c];
	}

	public function check(expectedIndex:Int, expectedGetterCalls:Int, expectedSetterCalls:Int, ?p:haxe.PosInfos) {
		eq(1, numCalled);
		eq(expectedIndex, index);
		eq(expectedGetterCalls, c.numGetterCalls);
		eq(expectedSetterCalls, c.numSetterCalls);
	}
}

private class PropertyAbstractTestContext extends unit.Test {
	public var index:Int;

	var numCalled:Int;

	var a:PropertyAbstract;

	public function new() {
		super();
		index = 0;
		numCalled = 0;
		a = new PropertyAbstract();
	}

	public function get() {
		numCalled++;
		return [a];
	}

	public function check(expectedIndex:Int, expectedGetterCalls:Int, expectedSetterCalls:Int, ?p:haxe.PosInfos) {
		eq(1, numCalled);
		eq(expectedIndex, index);
		eq(expectedGetterCalls, a.getThis().numGetterCalls);
		eq(expectedSetterCalls, a.getThis().numSetterCalls);
	}
}

private abstract ValueAbstract(Int) {
	public var prop(get, set):Int;

	public inline function new() {
		this = 0;
	}

	private inline function get_prop():Int {
		return this;
	}

	private inline function set_prop(value:Int):Int {
		return this = value;
	}
}

class Issue9746 extends unit.Test {
	function testClass() {
		#if (cs && fast_cast && erase_generics)
		#else
		var ctx = new PropertyClassTestContext();
		eq(2, ctx.get()[ctx.index++].propGet += 2);
		ctx.check(1, 0 /* TODO: I think this should be 1 */, 0);
		#end

		var ctx = new PropertyClassTestContext();
		eq(2, ctx.get()[ctx.index++].propSet += 2);
		ctx.check(1, 0, 1);

		var ctx = new PropertyClassTestContext();
		eq(2, ctx.get()[ctx.index++].propGetSet += 2);
		ctx.check(1, 1, 1);
	}

	function testAbstract() {
		var ctx = new PropertyAbstractTestContext();
		eq(2, ctx.get()[ctx.index++].propGetSet += 2);
		ctx.check(1, 1, 1);
	}

	function testValueAbstract() {
		var a = new ValueAbstract();
		function getA() {
			return [a];
		}

		var index = 0;
		eq(2, getA()[index++].prop += 2);
		eq(1, index);
		eq(0 /* value semantics */, a.prop);
		#if (cs && fast_cast && erase_generics)
		#else
		eq(2, a.prop += 2);
		eq(2, a.prop);
		#end
	}
}