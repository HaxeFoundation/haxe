package unit.issues;

class Issue4705 extends unit.Test {
	function test() {
		var a:Object = { foo: 12, bar: '13' };
		var actual = [for(f in a) f];
		actual.sort(Reflect.compare);
		aeq(['bar', 'foo'], actual);
	}
}

@:forward
abstract Object(Dynamic) from Dynamic to Dynamic {
	public function iterator ():Iterator<String> {
		var fields = Reflect.fields (this);
		if (fields == null) fields = [];
		return fields.iterator();
	}
}