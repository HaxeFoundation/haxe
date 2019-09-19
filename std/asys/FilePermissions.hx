package asys;

/**
	File permissions in specify whether a file can be read, written, or executed
	by its owner, its owning group, and everyone else. Instances of this type
	can be constructed by combining individual file permissions with the `|`
	operator:

	```haxe
	ReadOwner | WriteOwner | ReadGroup | ReadOthers
	```

	Alternatively, file permissions may be specified as a string with exactly 9
	characters, in the format `rwxrwxrwx`, where each letter may instead be a
	`-` character. The first three characters represent the permissions of the
	owner, the second three characters represent the permissions of the owning
	group, and the last three characters represent the permissions of everyone
	else.

	```haxe
	"rw-r--r--"
	```

	Finally, file permissions may be constructed from an octal representation
	using the `fromOctal` function.

	```haxe
	FilePermissions.fromOctal("644")
	```
**/
enum abstract FilePermissions(Int) {
	@:from public static function fromString(s:String):FilePermissions {
		inline function bit(cc:Int, expect:Int):Int {
			return (if (cc == expect)
				1;
			else if (cc == "-".code)
				0;
			else
				throw "invalid file permissions string");
		}
		switch (s.length) {
			case 9: // rwxrwxrwx
				return new FilePermissions(bit(s.charCodeAt(0), "r".code) << 8
					| bit(s.charCodeAt(1), "w".code) << 7
					| bit(s.charCodeAt(2), "x".code) << 6
					| bit(s.charCodeAt(3), "r".code) << 5
					| bit(s.charCodeAt(4), "w".code) << 4
					| bit(s.charCodeAt(5), "x".code) << 3
					| bit(s.charCodeAt(6), "r".code) << 2
					| bit(s.charCodeAt(7), "w".code) << 1
					| bit(s.charCodeAt(8), "x".code));
			case _:
				throw "invalid file permissions string";
		}
	}

	public static function fromOctal(s:String):FilePermissions {
		inline function digit(n:Int):Int {
			if (n >= "0".code && n <= "7".code) return n - "0".code;
			throw "invalid octal file permissions";
		}
		switch (s.length) {
			case 3: // 777
				return new FilePermissions(digit(s.charCodeAt(0)) << 6
					| digit(s.charCodeAt(1)) << 3
					| digit(s.charCodeAt(2)));
			case _:
				throw "invalid octal file permissions";
		}
	}

	var None = 0;
	var ExecuteOthers = 1 << 0;
	var WriteOthers = 1 << 1;
	var ReadOthers = 1 << 2;
	var ExecuteGroup = 1 << 3;
	var WriteGroup = 1 << 4;
	var ReadGroup = 1 << 5;
	var ExecuteOwner = 1 << 6;
	var WriteOwner = 1 << 7;
	var ReadOwner = 1 << 8;

	inline function new(value:Int)
		this = value;

	inline function get_raw():Int return this;

	@:op(A | B)
	inline function join(other:FilePermissions) return new FilePermissions(this | other.get_raw());
}
