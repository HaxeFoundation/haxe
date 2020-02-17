/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package cpp;

import sys.net.Socket;

@:buildXml('<include name="${HXCPP}/src/hx/libs/std/Build.xml"/>')
extern class NativeSocket {
	@:native("_hx_std_socket_init")
	public static function socket_init():Void;

	@:native("_hx_std_socket_new")
	public static function socket_new(udp:Bool):Dynamic;

	@:native("_hx_std_socket_new")
	public static function socket_new_ip(udp:Bool, ipv6:Bool):Dynamic;

	@:native("_hx_std_socket_close")
	public static function socket_close(handle:Dynamic):Void;

	@:native("_hx_std_socket_bind")
	public static function socket_bind(o:Dynamic, host:Int, port:Int):Void;

	@:native("_hx_std_socket_bind_ipv6")
	public static function socket_bind_ipv6(o:Dynamic, host:haxe.io.BytesData, port:Int):Void;

	@:native("_hx_std_socket_send_char")
	public static function socket_send_char(o:Dynamic, c:Int):Void;

	@:native("_hx_std_socket_send")
	public static function socket_send(o:Dynamic, buf:haxe.io.BytesData, p:Int, l:Int):Int;

	@:native("_hx_std_socket_recv")
	public static function socket_recv(o:Dynamic, buf:haxe.io.BytesData, p:Int, l:Int):Int;

	@:native("_hx_std_socket_recv_char")
	public static function socket_recv_char(o:Dynamic):Int;

	@:native("_hx_std_socket_write")
	public static function socket_write(o:Dynamic, buf:haxe.io.BytesData):Void;

	@:native("_hx_std_socket_read")
	public static function socket_read(o:Dynamic):haxe.io.BytesData;

	@:native("_hx_std_host_resolve_ipv6")
	public static function host_resolve_ipv6(host:String):haxe.io.BytesData;

	@:native("_hx_std_host_resolve")
	public static function host_resolve(host:String):Int;

	@:native("_hx_std_host_to_string")
	public static function host_to_string(ip:Int):String;

	@:native("_hx_std_host_to_string_ipv6")
	public static function host_to_string_ipv6(ipv6:haxe.io.BytesData):String;

	@:native("_hx_std_host_reverse")
	public static function host_reverse(host:Int):String;

	@:native("_hx_std_host_reverse_ipv6")
	public static function host_reverse_ipv6(ipv6:haxe.io.BytesData):String;

	@:native("_hx_std_host_local")
	public static function host_local():String;

	inline public static function host_local_ipv6():String
		return "::1";

	@:native("_hx_std_socket_connect")
	public static function socket_connect(o:Dynamic, host:Int, port:Int):Void;

	@:native("_hx_std_socket_connect_ipv6")
	public static function socket_connect_ipv6(o:Dynamic, host:haxe.io.BytesData, port:Int):Void;

	@:native("_hx_std_socket_listen")
	public static function socket_listen(o:Dynamic, n:Int):Void;

	@:native("_hx_std_socket_select")
	public static function socket_select(rs:Array<Dynamic>, ws:Array<Dynamic>, es:Array<Dynamic>, timeout:Dynamic):Array<Dynamic>;

	@:native("_hx_std_socket_fast_select")
	public static function socket_fast_select(rs:Array<Dynamic>, ws:Array<Dynamic>, es:Array<Dynamic>, timeout:Dynamic):Void;

	@:native("_hx_std_socket_accept")
	public static function socket_accept(o:Dynamic):Dynamic;

	@:native("_hx_std_socket_peer")
	public static function socket_peer(o:Dynamic):Array<Int>;

	@:native("_hx_std_socket_host")
	public static function socket_host(o:Dynamic):Array<Int>;

	@:native("_hx_std_socket_set_timeout")
	public static function socket_set_timeout(o:Dynamic, t:Dynamic):Void;

	@:native("_hx_std_socket_shutdown")
	public static function socket_shutdown(o:Dynamic, r:Bool, w:Bool):Void;

	@:native("_hx_std_socket_set_blocking")
	public static function socket_set_blocking(o:Dynamic, b:Bool):Void;

	@:native("_hx_std_socket_set_fast_send")
	public static function socket_set_fast_send(o:Dynamic, b:Bool):Void;

	@:native("_hx_std_socket_set_broadcast")
	public static function socket_set_broadcast(o:Dynamic, b:Bool):Void;

	@:native("_hx_std_socket_poll_alloc")
	public static function socket_poll_alloc(nsocks:Int):Dynamic;

	@:native("_hx_std_socket_poll_prepare")
	public static function socket_poll_prepare(pdata:Dynamic, rsocks:Array<Socket>, wsocks:Array<Socket>):Array<Array<Int>>;

	@:native("_hx_std_socket_poll_events")
	public static function socket_poll_events(pdata:Dynamic, timeout:Float):Void;

	@:native("_hx_std_socket_poll")
	public static function socket_poll(socks:Array<Socket>, pdata:Dynamic, timeout:Float):Array<Socket>;

	@:native("_hx_std_socket_send_to")
	public static function socket_send_to(o:Dynamic, buf:haxe.io.BytesData, p:Int, l:Int, inAddr:Dynamic):Int;

	@:native("_hx_std_socket_recv_from")
	public static function socket_recv_from(o:Dynamic, buf:haxe.io.BytesData, p:Int, l:Int, outAddr:Dynamic):Int;
}
