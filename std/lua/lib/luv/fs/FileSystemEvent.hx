package lua.lib.luv.fs;

@:luaRequire("luv")
extern class FileSystemEvent {
  static function new_fs_event() : FileSystemEvent;
  @:native("new_fs_event") function new() : Void;

  function start(path : String, options : StartOptions, cb : String->Bool->Void) : Int;
  function stop() : Int;
  function getpath() : String;
}

typedef StartOptions = {
  watch_entry : Bool,
  stat        : Bool,
  recursive   : Bool
}
