package lua.lib.environ;
@:luaRequire("environ.process")
extern class Environ {
	static function getenv(arg : String) : String;
	static function setenv(arg : String, value : String) : Bool;
	static var ENV : Table<String, String>;
}

