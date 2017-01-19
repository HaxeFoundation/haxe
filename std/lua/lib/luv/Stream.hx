package lua.lib.luv; 
import lua.lib.luv.net.Tcp;

@:luaRequire("luv")
extern class Stream extends Handle {
  function shutdown(?cb : Void->Void) : Int;
  function listen(backlog : Int, cb : String->String->Void) : Int;
  function accept(client_stream : Stream) : Int;
  function read_start(cb : String->String->Void) : Int;
  function read_stop() : Int;
  function write(data : StreamData, ?cb : String->Bool->Void) : Int;
  function write2(data : StreamData, send_handle : Tcp, cb: String->Bool->Void) : Int;
  function try_write(data : StreamData) : Int;
  function is_readable() : Bool;
  function is_writable() : Bool;
  function set_blocking(blocking : Bool) : Int;
}

typedef StreamData = haxe.extern.EitherType<String,Table<Int,String>>;
