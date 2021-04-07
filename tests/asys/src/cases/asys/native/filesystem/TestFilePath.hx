package cases.asys.native.filesystem;

import haxe.PosInfos;
import asys.native.filesystem.FsException;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;
import haxe.io.Path;

/**
 * INFO
 * Paths are checked for equality using `equalPaths`, which automatically translates
 * (back)slashes and ignores trailing slashes if needed.
 */
class TestFilePath extends FsTest {
	function expect<T>(value:T, ?pos:PosInfos) {
		return {value:value, pos:pos};
	}

	function check(cases:Map<String,{value:String,pos:PosInfos}>, subject:(Null<FilePath>)->Null<String>) {
		for(path => expected in cases)
			equalPaths(expected.value, subject(path), expected.pos);
	}

	function testCreatePath() {
		var cases:Map<String,{value:String,pos:PosInfos}> = [
			FilePath.createPath('path', 'to', 'file') => expect('path/to/file'),
			FilePath.createPath('path/', 'to', 'file') => expect('/to/file'),
			FilePath.createPath('path', '/to', 'file') => expect('/to/file'),
			FilePath.createPath('path', '', 'file') => expect('path/file'),
			FilePath.createPath(['path', 'to', 'file']) => expect('path/to/file'),
			FilePath.createPath(['path/', 'to', 'file']) => expect('path/to/file'),
			FilePath.createPath(['path', '', 'file']) => expect('path/file'),
			FilePath.createPath(['path', '/to', 'file']) => expect('/to/file'),
		];
		//TODO: I'm not sure about these
		if(isWindows) {
			cases[FilePath.createPath(['C:', 'file'])] = expect('C:file');
			cases[FilePath.createPath(['C:/', 'file'])] = expect('C:/file');
			cases[FilePath.createPath(['path', 'C:file'])] = expect('C:path/file'); //???
			cases[FilePath.createPath(['D:/path', 'C:file'])] = expect('C:path/file'); //??????
		}
		check(cases, p -> p);
	}

	function testOfString() {
		var s = '𠜎/aa😂/éé';
		var p:FilePath = s;
		equalPaths(s, p);

		if(isWindows) {
			//root of drive C
			var s = 'C:\\';
			var p1:FilePath = s;
			equalPaths('C:\\', p1);

			//current working directory of drive C
			var s = 'C:';
			var p2:FilePath = s;
			equalPaths('C:', p2);

			isFalse(p1 == p2);
		}

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
			isTrue(('C:\\something':FilePath).isAbsolute());
			isTrue(('\\':FilePath).isAbsolute());
			isFalse(('C:something':FilePath).isAbsolute());
		} else {
			isFalse(('\\':FilePath).isAbsolute());
		}
	}

	function testNormalize() {
		var cases = [
			'some/path' => expect('some/path'),
			'' => expect(''),
			'.' => expect(''),
			'./' => expect(''),
			'path/to/./../../non-existent/./file' => expect('non-existent/file'),
			'check///slashes/' => expect('check/slashes'),
			'./all/redundant/../..' => expect(''),
			'leaves/../non-redundant/../double-dots/../../..' => expect('../..'),
			'...' => expect('...'),
			'/absolute/path' => expect('/absolute/path')
		];
		if(isWindows) {
			cases['C:/absolute/../path'] = expect('C:/path');
			cases['C:/absolute/excessive/dots/../../../..'] = expect('C:/');
			cases['C:relative/.././'] = expect('C:');
			cases['C:relative/../excessive/dots/../../../..'] = expect('C:../..');
		}
		check(cases, p -> p.normalize());
	}

	function testAbsolute() {
		var cwd = Path.addTrailingSlash(Sys.getCwd());
		var cases = [
			'some/path' => expect(cwd + 'some/path'),
			'' => expect(cwd),
			'.' => expect(cwd + '.'),
			'non-existent/file' => expect(cwd + 'non-existent/file'),
			'/absolute/path' => expect('/absolute/path')
		];
		if(isWindows) {
			var currentDrive = cwd.substr(0, 1);
			cases['C:/absolute/path'] = expect('C:/absolute/path');
			cases[currentDrive + ':relative/path'] = expect(cwd + 'relative/path');
		}
		check(cases, p -> p.absolute());
	}

	function testParent() {
		var cases = [
			'file' => expect(null),
			'/file' => expect('/'),
			'./file' => expect('.'),
			'path/to/file' => expect('path/to'),
			'path/to/dir/' => expect('path/to'),
			'path/to///dir/' => expect('path/to'),
			'path/to/../file' => expect('path/to/..'),
			'path/to/..' => expect('path/to'),
			'path/to/.' => expect('path/to'),
			'.hidden' => expect(null),
			'.' => expect(null),
			'' => expect(null),
			'/' => expect(null),
			'\\' => expect(null)
		];
		if(isWindows) {
			cases['C:\\'] = expect(null);
			cases['C:'] = expect(null);
			cases['C:\\dir'] = expect('C:\\');
			cases['C:dir'] = expect(null);
		}
		check(cases, p -> p.parent());
	}

	function testAdd() {
		var p = FilePath.ofString('dir');
		equalPaths('dir/file', p.add('file'));
		equalPaths('/file', p.add('/file'));
		equalPaths('dir', p.add(''));
		equalPaths('dir', FilePath.ofString('').add(p));
	}
}
