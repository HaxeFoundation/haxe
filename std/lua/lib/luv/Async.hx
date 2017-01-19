package lua.lib.luv;

@:luaRequire("luv")
extern class Async extends Handle {
  static function new_async() : Async;
  @:native("new_async") function new() : Void;
  function send() : Int;
}
