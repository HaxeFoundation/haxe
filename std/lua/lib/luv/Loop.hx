package lua.lib.luv;

@:luaRequire("luv")
extern class Loop {
  static function loop_close() : Bool;
  static function run(?mode : String) : Bool;
  static function loop_alive() : Bool;
  static function stop() : Void;
  static function backend_fd() : Int;
  static function backend_timeout() : Int;
  static function now() : Int;
  static function update_time() : Void;
  static function walk(cb : Handle->Void) : Void;
}
