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
