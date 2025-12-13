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

package sys.net;

class AsyncSocket {

	var loop : haxe.EventLoop;
	var stream : hl.uv.Stream;
	var tcp : hl.uv.Tcp;
	var recv : haxe.io.Bytes;
	var onWriteCallb : Bool -> Void;

	public function new( ?loop : haxe.EventLoop ) {
		if( loop == null ) loop = haxe.EventLoop.current;
		this.loop = loop;
	}

	public function close() {
		stream?.close();
		tcp = null;
		stream = null;
	}

	public function write( bytes : haxe.io.Bytes, pos : Int = 0, len : Int = -1 ) {
		if( stream == null ) throw new haxe.io.Eof();
		// libuv tells us we should wait for callb before reusing buffer ?
		if( onWriteCallb == null ) onWriteCallb = (b) -> onWrite(!b);
		stream.write(bytes,onWriteCallb,pos,len);
	}

	function init(ssl) {
		tcp = new hl.uv.Tcp(@:privateAccess loop.getUVLoop());
		stream = tcp;
		recv = haxe.io.Bytes.alloc(0);
		@:privateAccess loop.wakeup();
		if( ssl )
			stream = new hl.uv.SSLStream(stream);
	}

	public function connect(host:Host, port:Int, ssl=false) {
		if( tcp != null ) throw new haxe.io.Eof();
		init(ssl);
		tcp.connect(host,port,function(b) {
			if( ssl )
				Std.downcast(stream,hl.uv.SSLStream).handshake(function(err) {
					if( err != null )
						onSSLError(err);
					onConnectRaw(err == null);
				});
			else
				onConnectRaw(b);
		});
	}

	function onConnectRaw(b : Bool) {
		if( !b ) {
			close();
			onDisconnect();
			return;
		}
		onConnect();
		if( stream == null ) return;
		stream.readStartRaw(function(data,len) {
			if( len < 0 ) {
				close();
				onDisconnect();
			} else if( len > 0 ) {
				@:privateAccess {
					recv.b = data;
					recv.length = len;
				}
				onData(recv, 0, len);
			}
		});
	}

	public function listen(connections:Int) {
		if( tcp == null ) throw new haxe.io.Eof();
		tcp.listen(connections,() -> onConnect());
	}

	public function bind(host:Host, port:Int, ?ssl : { hostname:String, key:sys.ssl.Key, cert:sys.ssl.Certificate } ) {
		if( tcp != null ) throw new haxe.io.Eof();
		init(ssl != null);
		tcp.bind(host, port);
	}

	public function accept():Null<AsyncSocket> {
		if( tcp == null ) throw new haxe.io.Eof();
		var stream = tcp.accept();
		if( stream == null )
			return null;
		var s = Type.createEmptyInstance(AsyncSocket);
		s.stream = stream;
		s.recv = haxe.io.Bytes.alloc(0);
		s.onConnectRaw(true);
		return s;
	}

	public function setFastSend(b:Bool) {
		if( tcp == null ) throw new haxe.io.Eof();
		tcp.noDelay(b);
	}

	public dynamic function onConnect() {
	}

	public dynamic function onDisconnect() {
	}

	public dynamic function onData( bytes : haxe.io.Bytes, pos : Int, len : Int ) {
	}

	public dynamic function onWrite( error : Bool ) {
		if( error ) {
			onDisconnect();
			close();
		}
	}

	public dynamic function onSSLError( msg : String ) {
		throw msg;
	}

	public function writeString( str : String ) {
		var buf = haxe.io.Bytes.ofString(str);
		write(buf, 0, buf.length);
	}

	public static function startServer( host, port, ?ssl, onClient ) {
		var s = new AsyncSocket();
		s.onDisconnect = function() {
			onClient(null);
		};
		s.onConnect = function() {
			onClient(s.accept());
		};
		s.bind(new Host(host), port, ssl);
		s.listen(1);
		return s;
	}

}
