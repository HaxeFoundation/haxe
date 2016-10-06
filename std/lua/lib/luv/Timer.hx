package lua.lib.luv;

@:luaRequire("luv")
extern class Timer extends Handle {
  static function new_timer() : Timer;
  @:native("new_timer") function new() : Void;

  function start(timeout : Int, repeat : Int, cb : Void->Void) : Int;
  function stop() : Int;
  function again() : Int;
  function set_repeat(repeat : Int) : Void;
  function get_repeat() : Int;
}
