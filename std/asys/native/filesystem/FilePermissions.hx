package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import haxe.exceptions.NotImplementedException;

/**
	Filesystem permissions.

	Note that this is not an octal number.
	For octal numbers use `FilePermissions.octal` method.
**/
abstract FilePermissions(Int) from Int to Int {
	/**
		Specify file access mode as octal digits.

		For example an octal access mode `0o0765`
		could be set as `FilePermissions.octal(0, 7, 6, 5)`

		@param s - sticky bit, SETUID, SETGUID
		@param u - permissions for file owner
		@param g - permissions for file group
		@param o - permissions for other users

		For possible values of `s` check https://en.wikipedia.org/wiki/Setuid

		Possible values for `u`, `g`, and `o`:
		0 - no permission
		1 - execute only
		2 - write only
		3 - write and execute
		4 - read only
		5 - read and execute
		6 - read and write
		7 - read, write, and execute
	**/
	static public inline function octal(s:Int, u:Int, g:Int, o:Int):FilePermissions {
		return 512 * s + 64 * u + 8 * g + 1 * o;
	}

	/**
		Same as `FilePermissions.octal` except required arguments are taken from
		respective positions of `mode` array.
		For example:
		```haxe
		var mode:FilePermissions = [0, 7, 6, 5];
		//is the same as
		var mode = FilePermissions.octal(0, 7, 6, 5);
		```

		`mode` should contain exactly four items, otherwise
		`haxe.exceptions.ArgumentException` is thrown.

		Thanks to Haxe optimizations this method does not allocate an array at
		run time if supplied with an array declaration.
	**/
	@:from static inline function fromOctal(mode:Array<Int>) {
		if(mode.length != 4) {
			throw new ArgumentException('mode', '"mode" array should contain exactly four items');
		}
		return octal(mode[0], mode[1], mode[2], mode[3]);
	}
}