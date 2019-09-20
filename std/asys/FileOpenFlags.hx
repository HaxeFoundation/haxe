package asys;

class FileOpenFlagsImpl {
	public static function fromString(flags:String):FileOpenFlags {
		return (switch (flags) {
			case "r": ReadOnly;
			case "r+": ReadWrite;
			case "rs+": ReadWrite | Sync;
			case "sr+": ReadWrite | Sync;
			case "w": Truncate | Create | WriteOnly;
			case "w+": Truncate | Create | ReadWrite;
			case "a": Append | Create | WriteOnly;
			case "a+": Append | Create | ReadWrite;
			case "wx": Truncate | Create | WriteOnly | Excl;
			case "xw": Truncate | Create | WriteOnly | Excl;
			case "wx+": Truncate | Create | ReadWrite | Excl;
			case "xw+": Truncate | Create | ReadWrite | Excl;
			case "ax": Append | Create | WriteOnly | Excl;
			case "xa": Append | Create | WriteOnly | Excl;
			case "as": Append | Create | WriteOnly | Sync;
			case "sa": Append | Create | WriteOnly | Sync;
			case "ax+": Append | Create | ReadWrite | Excl;
			case "xa+": Append | Create | ReadWrite | Excl;
			case "as+": Append | Create | ReadWrite | Sync;
			case "sa+": Append | Create | ReadWrite | Sync;
			case _: throw "invalid file open flags";
		});
	}
}


/**
	Flags used when opening a file with `asys.FileSystem.open` or other file
	functions. Specify whether the opened file:

	- will be readable
	- will be writable
	- will be truncated (all data lost) first
	- will be in append mode
	- will be opened exclusively by this process

	Instances of this type can be created by combining flags with the bitwise or
	operator:

	```haxe
	Truncate | Create | WriteOnly
	```

	Well-known combinations of flags can be specified with a string. The
	supported modes are: `r`, `r+`, `rs+`, `sr+`, `w`, `w+`, `a`, `a+`, `wx`,
	`xw`, `wx+`, `xw+`, `ax`, `xa`, `as`, `sa`, `ax+`, `xa+`, `as+`, `sa+`.
**/
@:native("asys.FileOpenFlagsImpl")
extern enum abstract FileOpenFlags(Int) {
	@:from public static function fromString(flags:String):FileOpenFlags;

	inline function new(value:Int)
		this = value;

	inline function get_raw():Int return this;

	@:op(A | B)
	inline function join(other:FileOpenFlags):FileOpenFlags return new FileOpenFlags(this | other.get_raw());

	// TODO: some of these don't make sense in Haxe-wrapped libuv
	var Append;
	var Create;
	var Direct;
	var Directory;
	var Dsync;
	var Excl;
	var NoAtime;
	var NoCtty;
	var NoFollow;
	var NonBlock;
	var ReadOnly;
	var ReadWrite;
	var Sync;
	var Truncate;
	var WriteOnly;
}
