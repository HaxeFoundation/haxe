package eval.luv;

import eval.integers.Int64;
import eval.integers.UInt64;

/**
	Relevant directories.

	@see https://aantron.github.io/luv/luv/Luv/Path
**/
extern class Path {
	/**
		Evaluates to the executable's path.
		It's always the path to the Haxe compiler.
	**/
	static function exePath():Result<NativeString>;

	/**
		Evaluates to the current working directory.
	**/
	static function cwd():Result<NativeString>;

	/**
		Changes the current working directory.
	**/
	static function chdir(dir:NativeString):Result<Result.NoData>;

	/**
		Evaluates to the path of the home directory.
	**/
	static function homedir():Result<NativeString>;

	/**
		Evaluates to the path of the temporary directory.
	**/
	static function tmpdir():Result<NativeString>;
}