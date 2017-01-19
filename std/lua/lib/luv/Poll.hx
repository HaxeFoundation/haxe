package lua.lib.luv;

@:luaRequire("luv")
extern class Poll extends Handle {
  static function new_poll() : Async;
  @:native("new_poll") function new() : Void;

  function start(?type : Int, ?cb : Void->Void ) : Int;
  function stop() : Int;
}
