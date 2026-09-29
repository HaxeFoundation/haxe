package unit.issues;

private class Base<T> {
	public var received : T;
	public var receivedInCtor : T;

	public function new(item : T) {
		onItem(item);
		receivedInCtor = received;
	}

	public dynamic function onItem(item : T) : Void {
		received = item;
	}

	public function trigger(item : T) {
		onItem(item);
	}
}

private class Sub extends Base<String> {
	public function new() {
		super("ctor");
	}

	override function onItem(item : String) {
		received = item.toUpperCase();
	}
}

private class Mid extends Base<String> {
}

private class SubOfMid extends Mid {
	public function new() {
		super("ctor");
	}

	override function onItem(item : String) {
		received = item.toUpperCase();
	}
}

private class NoCtorBase {
	public var received : String;
	public var receivedInCtor : String;

	public dynamic function onItem(item : String) : Void {
		received = item;
	}

	public function trigger(item : String) {
		onItem(item);
	}
}

private class SubOfNoCtor extends NoCtorBase {
	public function new() {
		trigger("ctor");
		receivedInCtor = received;
	}

	override function onItem(item : String) {
		received = item.toUpperCase();
	}
}

class Issue13043 extends Test {
	function testNoOverride() {
		var b = new Base<String>("ctor");
		eq("ctor", b.receivedInCtor);
		b.trigger("hi");
		eq("hi", b.received);
		b.onItem("ho");
		eq("ho", b.received);
	}

	function testOverrideAsSub() {
		var s = new Sub();
		eq("CTOR", s.receivedInCtor);
		s.trigger("hi");
		eq("HI", s.received);
		s.onItem("ho");
		eq("HO", s.received);

		s.onItem = function(item) { s.received = item + "!"; };
		s.trigger("hi");
		eq("hi!", s.received);
		s.onItem("ho");
		eq("ho!", s.received);
	}

	function testOverrideAsBase() {
		var b : Base<String> = new Sub();
		b.trigger("hi");
		eq("HI", b.received);
		b.onItem("ho");
		eq("HO", b.received);

		b.onItem = function(item) { b.received = item + "!"; };
		b.trigger("hi");
		eq("hi!", b.received);
		b.onItem("ho");
		eq("ho!", b.received);

		// assigning null must not wrap it into a non null closure
		#if (!lua && !cppia)
		b.onItem = null;
		t(b.onItem == null);
		#end
	}

	function testOverrideSkippingParent() {
		var b : Base<String> = new SubOfMid();
		eq("CTOR", b.receivedInCtor);
		b.trigger("hi");
		eq("HI", b.received);
		b.onItem("ho");
		eq("HO", b.received);

		// assigning through the subclass must reach the field of the declaring class
		var s = new SubOfMid();
		s.onItem = function(item) { s.received = item + "!"; };
		s.trigger("hi");
		eq("hi!", s.received);
		s.onItem("ho");
		eq("ho!", s.received);
	}

	function testOverrideWithoutParentCtor() {
		var b : NoCtorBase = new SubOfNoCtor();
		eq("CTOR", b.receivedInCtor);
		b.trigger("hi");
		eq("HI", b.received);
		b.onItem("ho");
		eq("HO", b.received);

		b.onItem = function(item) { b.received = item + "!"; };
		b.trigger("hi");
		eq("hi!", b.received);
		b.onItem("ho");
		eq("ho!", b.received);
	}
}
