package lua.lib.luasocket;
@:luaRequire("socket")
extern class Socket {
	public static function gettime() : Float;
}
