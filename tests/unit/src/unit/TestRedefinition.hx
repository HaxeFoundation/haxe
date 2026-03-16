package unit;

abstract class Dispatcher {
	public var scheduler(get, never):Float;

	abstract function get_scheduler():Float;
}

class ConcreteDispatcher extends Dispatcher {
	@:isVar public var scheduler(get, null):Int;

	public function new(v:Int) {
		this.scheduler = v;
	}

	function get_scheduler() {
		return scheduler;
	}
}

class ParentWithNonPhysical {
	public function new() {}

	public var name(get, private set):String;

	function get_name():String
		return "parent";

	function set_name(v:String):String
		return "parent";
}

class ChildWidened extends ParentWithNonPhysical {
	@:isVar public var name(get, set):String;

	public function new(v:String) {
		super();
		this.name = v;
	}

	override function get_name():String
		return name;

	override function set_name(v:String):String
		return name = v;
}

class ChildWidenedNonPhysical extends ParentWithNonPhysical {
	public var name(get, set):String;

	public function new() {
		super();
	}

	override function get_name():String
		return "child";

	override function set_name(v:String):String
		return v;
}

class TestRedefinition extends Test {
	public function testDispatcher() {
		final dispatcher = new ConcreteDispatcher(123);
		eq(dispatcher.scheduler, 123);
		final generalDispatcher:Dispatcher = dispatcher;
		eq(generalDispatcher.scheduler, 123.0);
	}

	public function testWidening() {
		final child = new ChildWidened("child");
		eq(child.name, "child");
		child.name = "new child";
		eq(child.name, "new child");

		final parent:ParentWithNonPhysical = child;
		eq(parent.name, "new child");
	}

	public function testWideningNonPhysical() {
		final child = new ChildWidenedNonPhysical();
		eq(child.name, "child");
		child.name = "new child";
		// Non-physical setter has no storage, so the value is unchanged
		eq(child.name, "child");

		final parent:ParentWithNonPhysical = child;
		eq(parent.name, "child");
	}
}
