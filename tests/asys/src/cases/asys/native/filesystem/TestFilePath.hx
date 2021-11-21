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
	function expect(value:FilePath, ?pos:PosInfos) {
		return {value:value, pos:pos};
	}

	inline function cases(m:Map<{value:FilePath,pos:PosInfos},Null<String>>) {
		return m;
	}

	function check(cases:Map<{value:FilePath,pos:PosInfos},Null<String>>, subject:(Null<FilePath>)->Null<String>) {
		for(path => expected in cases) {
			var actual = try {
				subject(path.value);
			} catch(e) {
				throw new haxe.exceptions.PosException(e.message, e, path.pos);
			}
			equalPaths(expected, actual, path.pos);
		}
	}

	function testCreatePath() {
		var cases = cases([
			expect(FilePath.createPath('path', 'to', 'file')) => 'path/to/file',
			expect(FilePath.createPath('path/', 'to', 'file')) => 'path/to/file',
			expect(FilePath.createPath('path', '/to', 'file')) => '/to/file',
			expect(FilePath.createPath('path', '', 'file')) => 'path/file',
			expect(FilePath.createPath(['path', 'to', 'file'])) => 'path/to/file',
			expect(FilePath.createPath(['path/', 'to', 'file'])) => 'path/to/file',
			expect(FilePath.createPath(['path', '', 'file'])) => 'path/file',
			expect(FilePath.createPath(['path', '/to', 'file'])) => '/to/file',
		]);
		if(isWindows) {
			cases[expect(FilePath.createPath('C:/', 'file'))] = 'C:/file';
			raises(() -> FilePath.createPath([]), ArgumentException);
			raises(() -> FilePath.createPath('D:/path', 'C:file'), ArgumentException);
			//TODO: I'm not sure about these
			// cases[expect('C:file')] = FilePath.createPath('C:', 'file');//???
			// cases[expect('C:path/file')] = FilePath.createPath('path', 'C:file'); //???
		}
		check(cases, p -> p);
	}
#if target.unicode
	function testOfString_unicode() {
		var s = '𠜎/aa😂/éé';
		var p = FilePath.ofString(s);
		equalPaths(s, p);
	}
#end
	function testOfArray() {
		#if target.unicode
			var p:FilePath = ['𠜎', '😂'];
			equalPaths('𠜎/😂', p);
		#else
			var p:FilePath = ['hello', 'world'];
			equalPaths('hello/world', p);
		#end
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
			expect('.') => '',
			expect('./') => '',
			expect('path/to/./../../non-existent/./file') => 'non-existent/file',
			expect('check///slashes/') => 'check/slashes',
			expect('./all/redundant/../..') => '',
			expect('leaves/../non-redundant/../double-dots/../../..') => '../..',
			expect('...') => '...',
			expect('/absolute/path') => '/absolute/path',
		]);
		if(isWindows) {
			cases[expect('C:/absolute/../path')] = 'C:/path';
			cases[expect('C:/back/to/root/../../..')] = 'C:/';
			cases[expect('C:/absolute/excessive/dots/../../../..')] = 'C:/';
			cases[expect('C:relative/.././')] = 'C:';
			cases[expect('C:relative/../excessive/dots/../../../..')] = 'C:../..';
		}
		check(cases, p -> p.normalize());
	}

	function testAbsolute() {
		var cwd = Path.addTrailingSlash(Sys.getCwd());
		var cases = cases([
			expect('some/path') => cwd + 'some/path',
			expect('') => cwd,
			expect('.') => cwd,
			expect('non-existent/file') => cwd + 'non-existent/file',
		]);
		if(isWindows) {
			var currentDrive = cwd.substr(0, 1);
			cases[expect('/absolute/path')] = '/absolute/path';
			cases[expect('C:/absolute/path')] = 'C:/absolute/path';
			cases[expect(cwd + 'relative/path')] = cwd + 'relative/path';
		} else {
			cases[expect('/absolute/path')] = '/absolute/path';
		}
		check(cases, p -> p.absolute());
	}

	function testParent() {
		var cases = cases([
			expect('file') => null,
			expect('/file') => '/',
			expect('./file') => '.',
			expect('path/to/file') => 'path/to',
			expect('path/to/dir/') => 'path/to',
			expect('path/to///dir/') => 'path/to',
			expect('path/to/../file') => 'path/to/..',
			expect('path/to/..') => 'path/to',
			expect('path/to/.') => 'path/to',
			expect('.hidden') => null,
			expect('.') => null,
			expect('') => null,
			expect('/') => null,
			expect('\\') => null,
		]);
		if(isWindows) {
			cases[expect('C:\\')] = null;
			cases[expect('C:')] = null;
			cases[expect('C:\\dir')] = 'C:\\';
			cases[expect('C:dir')] = 'C:';
			cases[expect('C:.\\dir')] = 'C:.';
		}
		check(cases, p -> p.parent());
	}

	function testName() {
		var cases = cases([
			expect('file.ext') => 'file.ext',
			expect('path/to/file.ext') => 'file.ext',
			expect('./file.ext') => 'file.ext',
			expect('path/to/dir/') => 'dir',
			expect('path/to/.') => '.',
			expect('') => '',
			expect('/') => '',
		]);
		if(isWindows) {
			cases[expect('C:\\')] = 'C:\\'; // TODO: Is this what we want? Or ''? Or 'C:'?
			cases[expect('C:')] = 'C:';
			cases[expect('C:\\file.ext')] = 'file.ext';
			cases[expect('C:\\dir\\')] = 'dir';
			cases[expect('C:dir')] = 'dir';
		}
		check(cases, p -> p.name());
	}

	function testAdd() {
		var dir = FilePath.ofString('dir');
		var cases = cases([
			expect(dir.add('file')) => 'dir/file',
			expect(dir.add('/file')) => '/file',
			expect(dir.add('')) => 'dir',
			expect(FilePath.ofString('').add(dir)) => 'dir',
		]);
		if(isWindows) {
			cases[expect(FilePath.ofString('C:/').add(dir))] = 'C:/dir';
		}
		check(cases, p -> p);
	}
}
