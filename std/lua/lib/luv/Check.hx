package lua.lib.luv;
@:luaRequire("luv")
extern class Check extends Handle {
  static function new_check() : Check;
  @:native("new_check") function new() : Void;

  function start(handle : Handle) : Int;
  function stop() : Int;
}
