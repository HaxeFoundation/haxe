package lua.lib.luv;

@:luaRequire("luv")
extern class Thread {
  static function new_thread() : Timer;
  @:native("new_thread") function new() : Void;

  static function self() : Thread;
  static function sleep(msec : Int) : Void;
  function equal(t : Thread) : Bool;
  function join(t : Thread) : Bool;
}
