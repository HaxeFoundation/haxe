package cases.issues;

class Issue9636 extends TestCase {
	function test(_) {
		vfs.putContent('Main.hx', getTemplate('issues/Issue9636/Main.hx'));
		var args = ['--main', 'Main', '--macro', 'include("pack")', '--js', 'test.js'];
		runHaxe(args);
		assertSuccess();
		vfs.putContent('pack/Foo.hx', getTemplate('issues/Issue9636/Foo.hx'));
		runHaxe(args);
		assertSuccess();
		runHaxe(['--cmd', 'node test.js']);
		Assert.isTrue(lastResult.stderr.contains('Exception from Foo'));
	}
}
