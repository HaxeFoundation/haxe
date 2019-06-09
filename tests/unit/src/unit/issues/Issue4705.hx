package unit.issues;

class Issue4705 extends unit.Test {
	function test() {
		var a:Object = { foo: 12, bar: '13' };
		aeq(['foo', 'bar'], [for(f in a) f]);
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