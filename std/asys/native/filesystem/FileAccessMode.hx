package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import haxe.exceptions.NotImplementedException;

enum abstract FileAccessBit(Int) to Int {
	/** File exists and is visible for the user */
	var Exists = 0;
	/** File can be executed */
	var Executable = 1;
	/** File can be written */
	var Writable = 2;
	/** File can be read */
	var Readable = 4;

	@:op(A | B) function joinBit(other:FileAccessBit):FileAccessMode;
	@:op(A | B) function joinMode(other:FileAccessMode):FileAccessMode;
}

/**
	Filesystem permissions.

	Note that this is not an octal number.
	For octal numbers use `FileAccessMode.octal` method.
**/
abstract FileAccessMode(Int) from Int to Int from FileAccessBit {
	/**
		Specify symbolic file access mode.

		TODO:
		The following doc is copied from man chomd.
		Rewrite to avoid legal issues.

		Format: `[ugoa...][[-+=][rwxXst...]...]`

		A combination of the letters ugoa controls which users' access to the
		file will be changed: the user who owns it (u), other users in the
		file's group (g), other users not in the file's group (o), or all users (a).
		If none of these are given, the effect is as if (a) were given.

		The letters rwxXst select file mode bits for the affected users: read (r),
		write (w), execute (or search for directories) (x), execute/search only if
		the file is a directory or already has execute permission for some user (X),
		set user or group ID on execution (s), restricted deletion flag or sticky bit (t).
		Instead of one or more of these letters, you can specify exactly one of
		the letters ugo: the permissions granted to the user who owns the file (u),
		the permissions granted to other users who are members of the file's group (g),
		and the permissions granted to users that are in neither of the two preceding
		categories (o).

		Example: `var mode:FileAccessMode = 'g+r-x';`
	**/
	@:from
	static public function symbolic(str:String):FileAccessMode {
		throw new NotImplementedException();
	}

	/**
		Specify file access mode as octal digits.

		For example an octal access mode `0o1765`
		could be set as `FileAccessMode.octal(1, 7, 6, 5)`

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
	static public function octal(s:Int, u:Int, g:Int, o:Int):FileAccessMode {
		throw new NotImplementedException();
	}

	/**
		Same as `FileAccessMode.octal` except required arguments are taken from
		respective positions of `mode` array.
		For example:
		```haxe
		var mode:FileAccessMode = [1, 7, 6, 5];
		//is the same as
		var mode = FileAccessMode.octal(1, 7, 6, 5);
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