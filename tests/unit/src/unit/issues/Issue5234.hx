package unit.issues;

class Issue5234 extends Test {
	function test() {
		var f = function(error){
			throw error;
		}
		try{
			f('test');
			t(false);
		} catch(e:Dynamic){
			t(true);
		}
	}
}
