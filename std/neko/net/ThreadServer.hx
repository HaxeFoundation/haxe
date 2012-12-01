/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package neko.net;

private typedef ThreadInfos = {
	var id : Int;
	var t : neko.vm.Thread;
	var p : neko.net.Poll;
	var socks : Array<sys.net.Socket>;
}

private typedef ClientInfos<Client> = {
	var client : Client;
	var sock : sys.net.Socket;
	var thread : ThreadInfos;
	var buf : haxe.io.Bytes;
	var bufpos : Int;
}

class ThreadServer<Client,Message> {

	var threads : Array<ThreadInfos>;
	var sock : sys.net.Socket;
	var worker : neko.vm.Thread;
	var timer : neko.vm.Thread;
	public var listen : Int;
	public var nthreads : Int;
	public var connectLag : Float;
	public var errorOutput : haxe.io.Output;
	public var initialBufferSize : Int;
	public var maxBufferSize : Int;
	public var messageHeaderSize : Int;
	public var updateTime : Float;
	public var maxSockPerThread : Int;

	public function new() {
		threads = new Array();
		nthreads = if( Sys.systemName() == "Windows" ) 150 else 10;
		messageHeaderSize = 1;
		listen = 10;
		connectLag = 0.5;
		errorOutput = Sys.stderr();
		initialBufferSize = (1 << 10);
		maxBufferSize = (1 << 16);
		maxSockPerThread = 64;
		updateTime = 1;
	}

	function runThread(t) {
		while( true ) {
			try {
				loopThread(t);
			} catch( e : Dynamic ) {
				logError(e);
			}
		}
	}

	function readClientData( c : ClientInfos<Client> ) {
		var available = c.buf.length - c.bufpos;
		if( available == 0 ) {
			var newsize = c.buf.length * 2;
			if( newsize > maxBufferSize ) {
				newsize = maxBufferSize;
				if( c.buf.length == maxBufferSize )
					throw "Max buffer size reached";
			}
			var newbuf = haxe.io.Bytes.alloc(newsize);
			newbuf.blit(0,c.buf,0,c.bufpos);
			c.buf = newbuf;
			available = newsize - c.bufpos;
		}
		var bytes = c.sock.input.readBytes(c.buf,c.bufpos,available);
		var pos = 0;
		var len = c.bufpos + bytes;
		while( len >= messageHeaderSize ) {
			var m = readClientMessage(c.client,c.buf,pos,len);
			if( m == null )
				break;
			pos += m.bytes;
			len -= m.bytes;
			work(callback(clientMessage,c.client,m.msg));
		}
		if( pos > 0 )
			c.buf.blit(0,c.buf,pos,len);
		c.bufpos = len;
	}

	function loopThread( t : ThreadInfos ) {
		if( t.socks.length > 0 )
			for( s in t.p.poll(t.socks,connectLag) ) {
				var infos : ClientInfos<Client> = s.custom;
				try {
					readClientData(infos);
				} catch( e : Dynamic ) {
					t.socks.remove(s);
					if( !Std.is(e,haxe.io.Eof) && !Std.is(e,haxe.io.Error) )
						logError(e);
					work(callback(doClientDisconnected,s,infos.client));
				}
			}
		while( true ) {
			var m : { s : sys.net.Socket, cnx : Bool } = neko.vm.Thread.readMessage(t.socks.length == 0);
			if( m == null )
				break;
			if( m.cnx )
				t.socks.push(m.s);
			else if( t.socks.remove(m.s) ) {
				var infos : ClientInfos<Client> = m.s.custom;
				work(callback(doClientDisconnected,m.s,infos.client));
			}
		}
	}

	function doClientDisconnected(s,c) {
		try s.close() catch( e : Dynamic ) {};
		clientDisconnected(c);
	}

	function runWorker() {
		while( true ) {
			var f = neko.vm.Thread.readMessage(true);
			try {
				f();
			} catch( e : Dynamic ) {
				logError(e);
			}
			try {
				afterEvent();
			} catch( e : Dynamic ) {
				logError(e);
			}
		}
	}

	public function work( f : Void -> Void ) {
		worker.sendMessage(f);
	}

	function logError( e : Dynamic ) {
		var stack = haxe.CallStack.exceptionStack();
		if( neko.vm.Thread.current() == worker )
			onError(e,stack);
		else
			work(callback(onError,e,stack));
	}

	function addClient( sock : sys.net.Socket ) {
		var start = Std.random(nthreads);
		for( i in 0...nthreads ) {
			var t = threads[(start + i)%nthreads];
			if( t.socks.length < maxSockPerThread ) {
				var infos : ClientInfos<Client> = {
					thread : t,
					client : clientConnected(sock),
					sock : sock,
					buf : haxe.io.Bytes.alloc(initialBufferSize),
					bufpos : 0,
				};
				sock.custom = infos;
				infos.thread.t.sendMessage({ s : sock, cnx : true });
				return;
			}
		}
		refuseClient(sock);
	}

	function refuseClient( sock : sys.net.Socket) {
		// we have reached maximum number of active clients
		sock.close();
	}

	function runTimer() {
		var l = new neko.vm.Lock();
		while( true ) {
			l.wait(updateTime);
			work(update);
		}
	}

	function init() {
		worker = neko.vm.Thread.create(runWorker);
		timer = neko.vm.Thread.create(runTimer);
		for( i in 0...nthreads ) {
			var t = {
				id : i,
				t : null,
				socks : new Array(),
				p : new neko.net.Poll(maxSockPerThread),
			};
			threads.push(t);
			t.t = neko.vm.Thread.create(callback(runThread,t));
		}
	}

	public function addSocket( s : sys.net.Socket ) {
		s.setBlocking(false);
		work(callback(addClient,s));
	}

	public function run( host, port ) {
		sock = new sys.net.Socket();
		sock.bind(new sys.net.Host(host),port);
		sock.listen(listen);
		init();
		while( true ) {
			try {
				addSocket(sock.accept());
			} catch( e : Dynamic ) {
				logError(e);
			}
		}
	}

	public function sendData( s : sys.net.Socket, data : String ) {
		try {
			s.write(data);
		} catch( e : Dynamic ) {
			stopClient(s);
		}
	}

	public function stopClient( s : sys.net.Socket ) {
		var infos : ClientInfos<Client> = s.custom;
		try s.shutdown(true,true) catch( e : Dynamic ) { };
		infos.thread.t.sendMessage({ s : s, cnx : false });
	}

	// --- CUSTOMIZABLE API ---

	public dynamic function onError( e : Dynamic, stack ) {
		var estr = try Std.string(e) catch( e2 : Dynamic ) "???" + try "["+Std.string(e2)+"]" catch( e : Dynamic ) "";
		errorOutput.writeString( estr + "\n" + haxe.CallStack.toString(stack) );
		errorOutput.flush();
	}

	public dynamic function clientConnected( s : sys.net.Socket ) : Client {
		return null;
	}

	public dynamic function clientDisconnected( c : Client ) {
	}

	public dynamic function readClientMessage( c : Client, buf : haxe.io.Bytes, pos : Int, len : Int ) : { msg : Message, bytes : Int } {
		return {
			msg : null,
			bytes : len,
		};
	}

	public dynamic function clientMessage( c : Client, msg : Message ) {
	}

	public dynamic function update() {
	}

	public dynamic function afterEvent() {
	}

}
