package cases.asys.native.filesystem;

import haxe.PosInfos;
import asys.native.filesystem.FsException;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;
import haxe.io.Path;
import haxe.exceptions.ArgumentException;
import haxe.exceptions.PosException;

/**
 * INFO
 * Paths are checked for equality using `equalPaths`, which automatically translates
 * (back)slashes and ignores trailing slashes if needed.
 */
class TestFilePath extends FsTest {
	function expect<T>(value:T, ?pos:PosInfos) {
		return {value:value, pos:pos};
	}

	inline function cases(m:Map<{value:String,pos:PosInfos},FilePath>) {
		return m;
	}

	function check(cases:Map<{value:String,pos:PosInfos},FilePath>, subject:(Null<FilePath>)->Null<String>) {
		for(expected => path in cases)
			equalPaths(expected.value, subject(path), expected.pos);
	}

	function testCreatePath() {
		var cases = cases([
			expect('path/to/file') => FilePath.createPath('path', 'to', 'file'),
			expect('path/to/file') => FilePath.createPath('path/', 'to', 'file'),
			expect('/to/file') => FilePath.createPath('path', '/to', 'file'),
			expect('path/file') => FilePath.createPath('path', '', 'file'),
			expect('path/to/file') => FilePath.createPath(['path', 'to', 'file']),
			expect('path/to/file') => FilePath.createPath(['path/', 'to', 'file']),
			expect('path/file') => FilePath.createPath(['path', '', 'file']),
			expect('/to/file') => FilePath.createPath(['path', '/to', 'file']),
		]);
		//TODO: I'm not sure about these
		if(isWindows) {
			cases[expect('C:file')] = FilePath.createPath('C:', 'file');
			cases[expect('C:/file')] = FilePath.createPath('C:/', 'file');
			cases[expect('C:path/file')] = FilePath.createPath('path', 'C:file'); //???
			raises(() -> FilePath.createPath('D:/path', 'C:file'), ArgumentException); //??????

			cases[expect('C:file')] = FilePath.createPath(['C:', 'file']);
			cases[expect('C:/file')] = FilePath.createPath(['C:/', 'file']);
			cases[expect('C:path/file')] = FilePath.createPath(['path', 'C:file']); //???
			raises(() -> FilePath.createPath(['D:/path', 'C:file']), ArgumentException); //??????
			raises(() -> FilePath.createPath([]), ArgumentException);
		}
		check(cases, p -> p);
	}

	function testOfString_unicode() {
		var s = '𠜎/aa😂/éé';
		var p = FilePath.ofString(s);
		equalPaths(s, p);
	}

	function testOfArray() {
		var p:FilePath = ['𠜎', '😂'];
		equalPaths('𠜎/😂', p);
	}

	function testEqual() {
		var p1 = FilePath.ofString('qwe');
		var p2 = FilePath.ofString('qwe');
		isTrue(p1 == p2);
	}

	function testIsAbsolute() {
		isTrue((Sys.getCwd():FilePath).isAbsolute());
		isTrue(('/something/something':FilePath).isAbsolute());
		isFalse(('':FilePath).isAbsolute());
		isFalse(('./':FilePath).isAbsolute());
		isFalse(('..':FilePath).isAbsolute());
		if(isWindows) {
			trace(('C:something':FilePath).toString()); // debug jvm
			isTrue(('C:\\something':FilePath).isAbsolute());
			isTrue(('\\':FilePath).isAbsolute());
			isFalse(('C:something':FilePath).isAbsolute());
		} else {
			isFalse(('\\':FilePath).isAbsolute());
		}
	}

	function testNormalize() {
		var cases = cases([
			expect('some/path') => 'some/path',
			expect('') => '',
			expect('') => '.',
			expect('') => './',
			expect('non-existent/file') => 'path/to/./../../non-existent/./file',
			expect('check/slashes') => 'check///slashes/',
			expect('') => './all/redundant/../..',
			expect('../..') => 'leaves/../non-redundant/../double-dots/../../..',
			expect('...') => '...',
			expect('/absolute/path') => '/absolute/path',
		]);
		if(isWindows) {
			cases[expect('C:/path')] = 'C:/absolute/../path';
			cases[expect('C:/')] = 'C:/absolute/excessive/dots/../../../..';
			cases[expect('C:')] = 'C:relative/.././';
			cases[expect('C:../..')] = 'C:relative/../excessive/dots/../../../..';
		}
		check(cases, p -> p.normalize());
	}

	function testAbsolute() {
		var cwd = Path.addTrailingSlash(Sys.getCwd());
		var cases = cases([
			expect(cwd + 'some/path') => 'some/path',
			expect(cwd) => '',
			expect(cwd + '.') => '.',
			expect(cwd + 'non-existent/file') => 'non-existent/file',
		]);
		if(isWindows) {
			var currentDrive = cwd.substr(0, 1);
			cases[expect(currentDrive + ':/absolute/path')] = '/absolute/path';
			cases[expect('C:/absolute/path')] = 'C:/absolute/path';
			cases[expect(cwd + 'relative/path')] = currentDrive + ':relative/path';
		} else {
			cases[expect('/absolute/path')] = '/absolute/path';
		}
		check(cases, p -> p.absolute());
	}

	function testParent() {
		var cases = cases([
			expect(null) => 'file',
			expect('/') => '/file',
			expect('.') => './file',
			expect('path/to') => 'path/to/file',
			expect('path/to') => 'path/to/dir/',
			expect('path/to') => 'path/to///dir/',
			expect('path/to/..') => 'path/to/../file',
			expect('path/to') => 'path/to/..',
			expect('path/to') => 'path/to/.',
			expect(null) => '.hidden',
			expect(null) => '.',
			expect(null) => '',
			expect(null) => '/',
			expect(null) => '\\',
		]);
		if(isWindows) {
			cases[expect(null)] = 'C:\\';
			cases[expect(null)] = 'C:';
			cases[expect('C:\\')] = 'C:\\dir';
			cases[expect('C:')] = 'C:dir';
		}
		check(cases, p -> p.parent());
	}

	function testAdd() {
		var dir = FilePath.ofString('dir');
		var cases = cases([
			expect('dir/file') => dir.add('file'),
			expect('/file') => dir.add('/file'),
			expect('dir') => dir.add(''),
			expect('dir') => FilePath.ofString('').add(dir),
		]);
		if(isWindows) {
			cases[expect('C:/dir')] = FilePath.ofString('C:/').add(dir);
		}
		check(cases, p -> p);
	}
}
