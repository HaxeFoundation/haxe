package lua.lib.luv.net;

@:luaRequire("luv")
extern class Udp extends Handle {
  static function new_udp() : Udp;
  @:native("new_udp") function new() : Void;

  function open(fd : Int) : Int;
  function bind(host : String, port : Int) : Int;
  function getsockname() : String;
  function set_membership(multicast_addr : String, interface_addr : String, membership : String) : Int;
  function set_multicast_loop(on : Bool) : Int;
  function set_multicast_ttl(ttl : Int ) : Int;
  function set_multicast_interface(interface_addr : String) : Int;
  function set_broadcast(on : Bool) : Int;
  function set_ttl(ttl : Int) : Int;
  function send(data : String, host : String, port : Int, cb : Bool->Void) : Int;
  function try_send(data : String, host : String, port : Int) : Int;
  function recv_start(cb : String->Bool->Void) : Int;
  function recv_stop() : Int;
}
