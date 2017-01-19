package lua.lib.luv.fs;
@:luaRequire("luv")
extern class FileSystemPoll {
  static function new_fs_poll() : FileSystemPoll;
  @:native("new_fs_poll") function new() : Void;

  function start(path : String, interval : Int, cb : String->Bool->Void) : Bool;
  function stop() : Bool;
  function getpath() : String;
}
