package lua.lib.luv;

@:luaRequire("luv")
extern class Signal extends Handle {
  static function new_signal() : Signal;
  @:native("new_signal") function new() : Void;

  function start(sigtype : haxe.extern.EitherType<Int,String>, ?cb : Void-> Void ) : Int;
  function stop() : Int;

}
