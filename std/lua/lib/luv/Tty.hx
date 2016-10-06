package lua.lib.luv;


@:luaRequire("luv")
extern class Tty extends Stream {
  static function new_tty(fd : Int, readable : Bool) : Tty;
  @:native("new_tty") function new(fd : Int, readable : Bool) : Void;

  static function reset_mode() : Int;

  function set_mode(mode : Int) : Int;
  function get_winsize() : WidthHeight;
}

@:multiReturn
extern class WidthHeight {
  var width : Int;
  var height : Int;
}
