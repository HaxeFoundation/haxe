package issues.aidan;

function someCall(v:Dynamic) {}

class Issue79 extends utest.Test {
	function test() {
		CoroRun.run(function() {
			someCall({
				var a = 1;
				someCall(a);
				a;
			});
		});
		Assert.pass();
	}
}