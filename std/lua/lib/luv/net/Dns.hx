package lua.lib.luv.net;

@:luaRequire("luv")
extern class Dns {

  @:overload(function(node : String, service : String, ?hints : AddrInfo, cb : String->Table<Int, AddrInfo>->Void) : Request {})
  public static function getaddrinfo(node : String, service : String, ?hints : AddrInfo ) : Table<Int,AddrInfo>;

  @:overload(function(ip: String, port : Int, family : String, cb : String->AddrInfo->Void) : Request {})
  public static function getnameinfo(info:AddrInfo) : String;
}

typedef AddrInfo = {
    ?ip       : String,
    ?addr     : String,
    ?port     : Int,
    ?family   : String,
    ?socktype : String
}
