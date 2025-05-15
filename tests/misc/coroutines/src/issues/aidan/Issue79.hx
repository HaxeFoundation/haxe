package issues.aidan;

function someCall(v:Dynamic) {}

class Issue79 extends utest.Test {
	function test() {
		Coroutine.run(@:coroutine.debug function() {
			someCall({
				var a = 1;
				someCall(a);
				a;
			});
		});
		Assert.pass();
	}
}