package unit.issues;

class Issue8876 extends Test {
	function test() {
		var foo:Person = {fullName: 'John Smith'};
		eq('John', foo.firstName);
	}
}

@:structInit
private class Person {
	public var fullName:String;
	public var firstName(get, never):String;
	function get_firstName():String return fullName.split(' ')[0];
}