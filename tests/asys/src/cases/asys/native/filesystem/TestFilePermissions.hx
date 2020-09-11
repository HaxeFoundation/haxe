package cases.asys.native.filesystem;

import haxe.PosInfos;
import asys.native.filesystem.FilePermissions;
import asys.native.filesystem.FilePermissions.octal;

class TestFilePermissions extends FsTest {
	inline function check(expected:FilePermissions, actual:FilePermissions, ?pos:PosInfos) {
		isTrue(expected == actual, 'Expected $expected but got $actual', pos);
	}

	function test() {
		check(668, octal(1, 2, 3, 4));
		check(438, octal(0, 6, 6, 6));
		check(493, octal(0, 7, 5, 5));
		check(493, octal(0, 7, 5, 5));

		check(668, [1, 2, 3, 4]);
		check(438, [0, 6, 6, 6]);
		check(493, [0, 7, 5, 5]);
		check(493, [0, 7, 5, 5]);
	}
}