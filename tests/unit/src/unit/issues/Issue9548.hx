package unit.issues;

import haxe.Constraints.Function;

private class ComponentBase {
    public function new() {
    }
}

private class ComponentImpl extends ComponentBase { }

private class ComponentBounds extends ComponentImpl { }

private class Component extends ComponentBounds {
    public function new() {
        super();
    }
}

class Issue9548 extends unit.Test {
	public function test() {
		utest.Assert.pass();
	}
}