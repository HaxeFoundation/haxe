package lua.lib.lpeg;

import lua.lib.lpeg.Pattern;

@:luaRequire("re")
extern class Re {
    public static function compile(str : String, ?def : Dynamic) : AbstractPattern;
}

