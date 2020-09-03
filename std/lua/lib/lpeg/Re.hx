package lua.lib.lpeg;

import lua.lib.lpeg.Pattern;

@:luaRequire("re")
extern class Re {
    public static function compile(str : String, ?def : Dynamic) : Pattern;
    public static function match(subject : String, pattern : Pattern, ?index : Int) : String;
    public static function find(subject : String, pattern : Pattern, ?index : Int) : Int;
    public static function gsub(subject : String, pattern : Pattern, replace : String) : String;
    public static function updatelocale() : Void;
}
