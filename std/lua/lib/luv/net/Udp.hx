/*
 * Copyright (C)2005-2017 Haxe Foundation
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
