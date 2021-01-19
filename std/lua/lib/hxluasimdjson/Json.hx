package lua.lib.hxluasimdjson;
@:luaRequire("hxsimdjson")
extern class Json {
    public static function parse(str:String) : Dynamic;
}
