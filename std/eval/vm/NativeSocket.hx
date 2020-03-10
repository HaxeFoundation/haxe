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

package eval.vm;

import sys.net.Socket;

extern class NativeSocket {
	function new():Void;
	function accept():NativeSocket;
	function bind(host:Int, port:Int):Void;
	function close():Void;
	function connect(host:Int, port:Int):Void;
	function host():{ip:Int, port:Int};
	function listen(connections:Int):Void;
	function peer():{ip:Int, port:Int};
	function receive(buf:haxe.io.Bytes, pos:Int, len:Int):Int;
	function receiveChar():Int;
	function send(buf:haxe.io.Bytes, pos:Int, len:Int):Int;
	function sendChar(char:Int):Void;
	function setFastSend(b:Bool):Void;
	function setTimeout(timeout:Float):Void;
	function shutdown(read:Bool, write:Bool):Void;

	static function select(read:Array<Socket>, write:Array<Socket>, others:Array<Socket>,
		?timeout:Float):{read:Array<Socket>, write:Array<Socket>, others:Array<Socket>};
}
