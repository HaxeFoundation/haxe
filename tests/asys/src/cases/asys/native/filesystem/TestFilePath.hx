package cases.asys.native.filesystem;

import haxe.PosInfos;
import asys.native.filesystem.FsException;
import haxe.io.Bytes;
import asys.native.filesystem.FilePath;
import haxe.io.Path;

class TestFilePath extends FsTest {
	function mk<T>(value:T, ?pos:PosInfos) {
		return {value:value, pos:pos};
	}

	function testEqual() {
		var p1 = FilePath.fromString('qwe');
		var p2 = FilePath.fromString('qwe');
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
		} else {
			isFalse(('\\':FilePath).isAbsolute());
		}
	}

	function testAbsolute() {
		inline function check(cases:Map<String,{value:String,pos:PosInfos}>) {
			for(path => expected in cases)
				equals(expected.value, (path:FilePath).absolute().toString(), expected.pos);
		}
		var cwd = Sys.getCwd();

		var cases = [
			'.' => mk(Path.removeTrailingSlashes(cwd)),
			'./' => mk(Path.removeTrailingSlashes(cwd)),
			'non-existent.file' => mk(cwd + 'non-existent.file'),
			'path/to/../../non-existent.file' => mk(cwd + 'non-existent.file'),
			'single-dot-before-double-dot/./../non-existent.file' => mk(cwd + 'non-existent.file'),
			'path/to/../' => mk(cwd + 'path'),
			'...' => mk(cwd + '...')
		];
		check(cases);
		cases = if(isWindows) {
			[
				'/absolute/path' => mk('\\absolute\\path'),
				'C:\\absolute\\path' => mk('C:\\absolute\\path')
			];
		} else {
			[
				'/absolute/path' => mk('/absolute/path')
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
				equals(expected.value, str, expected.pos);
			}
		}
		var cwd = Path.removeTrailingSlashes(Sys.getCwd());

		var cases = [
			'file' => mk(cwd),
			'path/to/file' => mk('path/to'),
			'path/to/dir/' => mk('path/to'),
			'path/to/../file' => mk('path/to/..'),
			'.' => mk(cwd),
			'./' => mk(cwd),
			'' => mk(cwd),
			'/' => mk(null)
		];
		if(isWindows) {
			cases['C:\\'] = mk(null);
			cases['\\'] = mk(null);
		} else {
			cases['\\'] = mk(cwd);
		}
		check(cases);
	}

	function specFromString_toString() {
		var s = '𠜎/aa😂/éé';
		var p:FilePath = s;
		s == p.toString();

		var s = 'some/dir/';
		var p:FilePath = s;
		'some/dir' == p.toString();

		var s = '/';
		var p:FilePath = s;
		'/' == p.toString();

		var s = '';
		var p:FilePath = s;
		'.' == p.toString();
	}
}
