package eval.luv;

/**
	Version information for the vendored libuv.

	@see https://aantron.github.io/luv/luv/Luv/Version
**/
extern class Version {
	/**
		Returns the libuv version as a string.
	**/
	static function string():String;

	/**
		libuv major version number.
	**/
	static final major:Int;

	/**
		libuv minor version number.
	**/
	static final minor:Int;

	/**
		libuv patch version number.
	**/
	static final patch:Int;

	/**
		`true` if the libuv version is a release, and `false` if it is a development version.
		This does not depend on Haxe compilation arguments and will almost always be `true`.
	**/
	static final isRelease:Bool;

	/**
		libuv version suffix for development releases.
	**/
	static final suffix:String;

	/**
		libuv version packed into a single integer.
	**/
	static final hex:Int;
}