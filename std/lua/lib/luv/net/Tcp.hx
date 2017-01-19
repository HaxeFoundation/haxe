package lua.lib.luv.net; 

@:luaRequire("luv")
extern class Tcp extends Stream {
  static function new_tcp() : Tcp;
  @:native("new_tcp") function new() : Void;
  function open(sock : Int) : Int;
  function nodelay(enable : Bool) : Int;
  function keepalive(enable : Bool, ?delay : Int) : Int;
  function simultaneous_accepts(enable : Bool) : Int;
  function bind(address : String, port : Int) : Int;
  function getsockname() : Int;
  function getpeername() : String;
  function connect(host : String, port : Int, cb : String->Bool->Void) : Int;
  function write_queue_size() : Int;
}
