package unit.issues;

class Issue7890 extends unit.Test {
	function test() {
		#if js
		var p = js.Promise.resolve('hi');

		p.then(
			(string) -> {
				return;
			}, (error) -> {
				trace(error);
			});
		#end
	}
}