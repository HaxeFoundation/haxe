/*
 * Copyright (C)2005-2018 Haxe Foundation
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

package lua.lib.luasocket.socket;
import haxe.extern.EitherType;

extern class TcpClient extends Socket {
	public function getpeername() :  AddrInfo;
	public function receive(pattern : EitherType<ReceivePattern,Int>, ?prefix : String) : Result<String>;
	public function send(data : String, ?i : Int, ?j : Int) : Result<Int>;
	public function shutdown(mode : ShutdownMode) : Result<Int>;
	public function settimeout(value  : Float, ?mode : TimeoutMode) : Void;
	public function setoption(option : TcpOption , value : EitherType<Bool, {on : Bool, timeout : Float}>) : Void;
}


