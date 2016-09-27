package unit.issues;

class Issue5016 extends Test {
	function test() {
		#if !cpp
		var obj = {abc:null, def:1}
		Reflect.deleteField(obj, 'def');
		f(Reflect.hasField(obj, 'def'));
		#end
	}
}