package lua.lib.luv;

@:luaRequire("luv")
extern class Work {
  static function new_work() : Work;
  @:native("new_work") function new() : Void;
  static function queue_work(work : Work) : Bool;
}
