package unit.issues;

class Issue7233 extends unit.Test {
	function test() {
		var data = {name: "Test"};
		var obj = Type.createInstance(TestClass, [data]);

		eq("Test", obj.name);
	}
}

private class TestClass {
    public var name : String;

    public function new(data) {
        this.name = data.name;
    }
}
