package lua.lib.luv;

@:luaRequire("luv")
extern class Handle {
  function is_active() : Bool;
  function is_closing() : Bool;
  function close() : Void;
  function ref() : Void;
  function unref() : Void;
  function has_ref() : Bool;
  function send_buffer_size(size : Int) : Int;
  function recv_buffer_size(size : Int) : Int;
  function fileno() : Int;
}
