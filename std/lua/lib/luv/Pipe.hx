package lua.lib.luv;
import haxe.extern.EitherType;

@:luaRequire("luv")
extern class Pipe extends Stream {
  static function new_pipe(ipc : Bool) : Pipe;
  @:native("new_pipe") function new(ipc : Bool) : Void;

  function open(file : EitherType<FileHandle,Handle>) : Pipe;
  function bind(name : String) : Pipe;
  function connect(name : String, cb: String->Bool->Void) : Int;
  function getsockname() : String;
  function pending_instances(count : Int) : Int;
  function pending_count() : Int;
  function pending_type() : Int;
}
