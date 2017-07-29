package cpp;

import sys.net.Socket;

@:buildXml("<include name=\"${HXCPP}/src/hx/libs/std/Build.xml\"/>")
extern class NativeSocket
{
   @:extern @:native("_hx_std_socket_init")
   public static function socket_init() : Void { }


   @:extern @:native("_hx_std_socket_new")
   public static function socket_new(udp:Bool) : Dynamic return null;

   @:extern @:native("_hx_std_socket_new")
   public static function socket_new_ip(udp:Bool,ipv6:Bool) : Dynamic return null;


   @:extern @:native("_hx_std_socket_close")
   public static function socket_close(handle:Dynamic) : Void { }


   @:extern @:native("_hx_std_socket_bind")
   public static function socket_bind(o:Dynamic,host:Int,port:Int) : Void { }

   @:extern @:native("_hx_std_socket_bind_ipv6")
   public static function socket_bind_ipv6(o:Dynamic,host:haxe.io.BytesData,port:Int) : Void { }


   @:extern @:native("_hx_std_socket_send_char")
   public static function socket_send_char(o:Dynamic,c:Int) : Void { }


   @:extern @:native("_hx_std_socket_send")
   public static function socket_send(o:Dynamic,buf:haxe.io.BytesData,p:Int,l:Int) : Int return 0;


   @:extern @:native("_hx_std_socket_recv")
   public static function socket_recv(o:Dynamic,buf:haxe.io.BytesData,p:Int,l:Int) : Int return 0;


   @:extern @:native("_hx_std_socket_recv_char")
   public static function socket_recv_char(o:Dynamic) : Int return 0;


   @:extern @:native("_hx_std_socket_write")
   public static function socket_write(o:Dynamic,buf:haxe.io.BytesData) : Void { }


   @:extern @:native("_hx_std_socket_read")
   public static function socket_read(o:Dynamic) : haxe.io.BytesData return null;

   @:extern @:native("_hx_std_host_resolve_ipv6")
   public static function host_resolve_ipv6(host:String) : haxe.io.BytesData return null;


   @:extern @:native("_hx_std_host_resolve")
   public static function host_resolve(host:String) : Int return 0;


   @:extern @:native("_hx_std_host_to_string")
   public static function host_to_string(ip:Int) : String return null;

   @:extern @:native("_hx_std_host_to_string_ipv6")
   public static function host_to_string_ipv6(ipv6:haxe.io.BytesData) : String return null;


   @:extern @:native("_hx_std_host_reverse")
   public static function host_reverse(host:Int) : String return null;

   @:extern @:native("_hx_std_host_reverse_ipv6")
   public static function host_reverse_ipv6(ipv6:haxe.io.BytesData) : String return null;


   @:extern @:native("_hx_std_host_local")
   public static function host_local() : String return null;

   inline public static function host_local_ipv6() : String return "::1";


   @:extern @:native("_hx_std_socket_connect")
   public static function socket_connect(o:Dynamic,host:Int,port:Int) : Void { }

   @:extern @:native("_hx_std_socket_connect_ipv6")
   public static function socket_connect_ipv6(o:Dynamic,host:haxe.io.BytesData,port:Int) : Void { }


   @:extern @:native("_hx_std_socket_listen")
   public static function socket_listen(o:Dynamic,n:Int) : Void { }


   @:extern @:native("_hx_std_socket_select")
   public static function socket_select(rs:Array<Dynamic>,ws:Array<Dynamic>,es:Array<Dynamic>,timeout:Dynamic) : Array<Dynamic> return null;


   @:extern @:native("_hx_std_socket_fast_select")
   public static function socket_fast_select(rs:Array<Dynamic>,ws:Array<Dynamic>,es:Array<Dynamic>,timeout:Dynamic) : Void { }


   @:extern @:native("_hx_std_socket_accept")
   public static function socket_accept(o:Dynamic) : Dynamic return null;


   @:extern @:native("_hx_std_socket_peer")
   public static function socket_peer(o:Dynamic) : Array<Int> return null;


   @:extern @:native("_hx_std_socket_host")
   public static function socket_host(o:Dynamic) : Array<Int> return null;


   @:extern @:native("_hx_std_socket_set_timeout")
   public static function socket_set_timeout(o:Dynamic,t:Dynamic) : Void { }


   @:extern @:native("_hx_std_socket_shutdown")
   public static function socket_shutdown(o:Dynamic,r:Bool,w:Bool) : Void { }


   @:extern @:native("_hx_std_socket_set_blocking")
   public static function socket_set_blocking(o:Dynamic,b:Bool) : Void { }


   @:extern @:native("_hx_std_socket_set_fast_send")
   public static function socket_set_fast_send(o:Dynamic,b:Bool) : Void { }


   @:extern @:native("_hx_std_socket_poll_alloc")
   public static function socket_poll_alloc(nsocks:Int) : Dynamic return null;


   @:extern @:native("_hx_std_socket_poll_prepare")
   public static function socket_poll_prepare(pdata:Dynamic,rsocks:Array<Socket>,wsocks:Array<Socket>) : Array< Array<Int> > return null;


   @:extern @:native("_hx_std_socket_poll_events")
   public static function socket_poll_events(pdata:Dynamic,timeout:Float) : Void { }


   @:extern @:native("_hx_std_socket_poll")
   public static function socket_poll(socks:Array<Socket>,pdata:Dynamic,timeout:Float) : Array<Socket> return null;


   @:extern @:native("_hx_std_socket_send_to")
   public static function socket_send_to(o:Dynamic,buf:haxe.io.BytesData,p:Int,l:Int,inAddr:Dynamic) : Int return 0;


   @:extern @:native("_hx_std_socket_recv_from")
   public static function socket_recv_from(o:Dynamic,buf:haxe.io.BytesData,p:Int,l:Int,outAddr:Dynamic) : Int return 0;



}


