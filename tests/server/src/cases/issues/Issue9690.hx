package cases.issues;

class Issue9690 extends TestCase {
	function test(_) {
		vfs.putContent('B.hx', getTemplate('issues/Issue9690/B.hx'));
		vfs.putContent('Main.hx', getTemplate('issues/Issue9690/Main.hx.inline'));
		var args = ['--main', 'Main', '-D', 'analyzer-optimize', '--js', 'test.js'];
		runHaxe(args);
		assertSuccess();
		vfs.putContent('Main.hx', getTemplate('issues/Issue9690/Main.hx.no-inline'));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath('Main.hx')});
		runHaxe(args);
		assertSuccess();
		runHaxe(['--cmd', 'node test.js']);
		Assert.isTrue(lastResult.hasError);
		Assert.isTrue(lastResult.stderr.contains('Error: side effect!'));
	}
}