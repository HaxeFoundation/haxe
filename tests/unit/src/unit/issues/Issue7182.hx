package unit.issues;

@:structInit
private class Person {
    public final name:String;
	public final hobbies = "compiler errors";
}

class Issue7182 extends unit.Test {
	function test() {
		var person:Person = {
			name: "nadako"
		}
		eq("nadako", person.name);
		eq("compiler errors", person.hobbies);
	}
}