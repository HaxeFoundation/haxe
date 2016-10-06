package lua.lib.luv;

@:luaRequire("luv")
extern class Prepare extends Handle {
  static function new_prepare() : Prepare;
  @:native("new_prepare") function new() : Void;

  function start() : Int;
  function stop() : Int;
}
