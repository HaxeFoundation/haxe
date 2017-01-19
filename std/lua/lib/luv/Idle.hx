package lua.lib.luv;

@:luaRequire("luv")
extern class Idle extends Handle {
  static function new_idle() : Idle;
  @:native("new_idle") function new() : Void;
  function start(cb : Void->Void) : Int;
  function stop(cb : Void->Void) : Int;
}
