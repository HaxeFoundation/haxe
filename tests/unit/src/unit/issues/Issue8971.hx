package unit.issues;

class Issue8971 extends unit.Test {
	function test() {
		var obj = getObject();
		eq('a', obj.a);
		eq('b', obj.b);
	}

	function getObject():{a:String, b:String} {
		return {
			a: cast('a', String),
			b: cast('b', String)
		}
	}
}