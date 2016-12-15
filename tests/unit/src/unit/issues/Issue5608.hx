package unit.issues;

class Issue5608 extends unit.Test {
	var fields:Array<String>;

	function test() {
		//should not throw
		fields = Reflect.fields(haxe.Serializer);
		t(true);
	}
}