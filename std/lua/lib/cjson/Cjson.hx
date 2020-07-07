package lua.lib.cjson;

@:luaRequire("cjson")
extern class Cjson {
    public static function encode(obj:Dynamic) : String;
    public static function decode(str:String) : Dynamic;
    public static function decode_array_with_array_mt(?enabled:Bool) : Void;
}

