package cases.asys.native.filesystem;

import haxe.PosInfos;
import asys.native.filesystem.FsException;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;
import haxe.io.Path;

class TestFilePath extends FsTest {
	function expect<T>(value:T, ?pos:PosInfos) {
		return {value:value, pos:pos};
	}

	function testEqual() {
		var p1 = FilePath.ofString('qwe');
		var p2 = FilePath.ofString('qwe');
		isTrue(p1 == p2);

		var p1 = FilePath.ofString('');
		var p2 = FilePath.ofString('.');
		isTrue(p1 == p2);

		var p1 = FilePath.ofString('some');
		var p2 = FilePath.ofString('some/');
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

	function testAbsolute() {
		inline function check(cases:Map<String,{value:String,pos:PosInfos}>) {
			for(path => expected in cases)
				equalPaths(expected.value, (path:FilePath).absolute().toString(), expected.pos);
		}
		var cwd = Path.addTrailingSlash(Sys.getCwd());

		var cases = [
			'.' => expect(Path.removeTrailingSlashes(cwd)),
			'./' => expect(Path.removeTrailingSlashes(cwd)),
			'non-existent.file' => expect(cwd + 'non-existent.file'),
			'path/to/../../non-existent.file' => expect(cwd + 'non-existent.file'),
			'single-dot-before-double-dot/./../non-existent.file' => expect(cwd + 'non-existent.file'),
			'path/to/../' => expect(cwd + 'path'),
			'...' => expect(cwd + '...')
		];
		check(cases);
		cases = if(isWindows) {
			var currentDrive = cwd.substr(0, 2);
			[
				'/absolute/path' => expect('\\absolute\\path'),
				'C:\\absolute\\path' => expect('C:\\absolute\\path'),
				'$currentDrive:relative\\path' => expect(cwd + 'relative\\path')
			];
		} else {
			[
				'/absolute/path' => expect('/absolute/path')
			];
		}
		check(cases);
	}

	function testParent() {
		inline function check(cases:Map<String,{value:Null<String>,pos:PosInfos}>) {
			for(path => expected in cases) {
				var str = switch (path:FilePath).parent() {
					case null: null;
					case parent: parent.toString();
				}
				equalPaths(expected.value, str, expected.pos);
			}
		}

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
		check(cases);
	}

	function testFromString_toString() {
		var s = '𠜎/aa😂/éé';
		var p:FilePath = s;
		equalPaths(s, p.toString());

		var s = 'some/dir///';
		var p:FilePath = s;
		equalPaths('some/dir', p.toString());

		var s = '/';
		var p:FilePath = s;
		equalPaths('/', p.toString());

		var s = '';
		var p:FilePath = s;
		equalPaths('.', p.toString());

		if(isWindows) {
			var s = 'some/dir/\\/';
			var p:FilePath = s;
			equalPaths('some/dir', p.toString());

			var s = '\\';
			var p:FilePath = s;
			equalPaths('\\', p.toString());

			//root of drive C
			var s = 'C:\\';
			var p:FilePath = s;
			equalPaths('C:\\', p.toString());

			var s = 'C:\\\\\\';
			var p:FilePath = s;
			equalPaths('C:\\', p.toString());

			//current working directory of drive C
			var s = 'C:';
			var p:FilePath = s;
			equalPaths('C:', p.toString());
		}

	}
}
