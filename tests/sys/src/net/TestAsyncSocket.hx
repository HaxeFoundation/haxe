package net;

import sys.net.*;
import utest.Assert;

class TestAsyncSocket extends utest.Test {

	var useMT : Bool;

	public function new( useMT = true ) {
		this.useMT = useMT;
		super();
	}

	public function testAsync() {
		var done = false;
		var host = new sys.net.Host("127.0.0.1");
		var port = 6669;
		var bigData = haxe.io.Bytes.alloc(32 << 20);
		var bigCount = bigData.length>>2;
		for( i in 0...bigCount )
			bigData.setInt32(i<<2,i);

		var lock = new sys.thread.Mutex();
		inline function log(msg) {
			if( false ) {
				lock.acquire();
				Sys.println(msg);
				lock.release();
			}
		}

		function startClient(cid:Int) {
			var c = new sys.net.AsyncSocket();
			log('[$cid] Connecting...');
			c.connect(host, port);
			var sendId = Std.random(100);
			var recvId = sendId;
			var lastId = sendId + 1 + Std.random(1000);
			var buf = haxe.io.Bytes.alloc(4);
			function send() {
				if( sendId >= lastId ) return false;
				log('[$cid] > $sendId');
				buf.setInt32(0, sendId++);
				c.write(buf, 0, 4);
				return true;
			}
			c.onDisconnect = function() {
				throw "Disconnected";
			};
			c.onConnect = function() {
				log('[$cid] Connected to server');
				var split = Std.random(10);
				var size = bigData.length >> split;
				var tmp = haxe.io.Bytes.alloc(size);
				for( i in 0...1<<split ) {
					tmp.blit(0, bigData, size*i, size);
					c.write(tmp, 0, size);
					Sys.sleep(0.001);
				}
				for( i in 0...10 )
					send();
			};
			c.onData = function(data,pos,len) {
				while( len >= 4 ) {
					var id = data.getInt32(pos);
					log('[$cid] < $id');
					if( id != recvId )
						throw "assert "+id+"!="+recvId;
					recvId++;
					pos += 4;
					len -= 4;
				}
				if( !send() ) {
					log('[$cid] Done');
					c.close();
				}
			};
		}

		function startServer() {
			var s = new sys.net.AsyncSocket();
			var connected = 0;
			s.bind(host, port);
			s.listen(10);
			log("Started");
			s.onConnect = function() {
				var cl = s.accept();
				var recv = -1, send = -1;
				var b = haxe.io.Bytes.alloc(4);
				var cid = connected++;
				var bigWait = 0;
				var remain = null;
				function addRemain(bytes,pos,len) {
					if( len > 0 ) remain = bytes.sub(pos,len);
				}
				log('[$cid] Connected');
				cl.onData = function(bytes, pos, len) {
					if( len == 0 )
						throw "assert";
					if( remain != null ) {
						var nbytes = haxe.io.Bytes.alloc(remain.length + len);
						nbytes.blit(0, remain, 0, remain.length);
						nbytes.blit(remain.length, bytes, pos, len);
						pos = 0;
						len += remain.length;
						remain = null;
						bytes = nbytes;
					}
					var hasBig = false;
					while( len >= 4 && bigWait < bigCount ) {
						var id = bytes.getInt32(pos);
						if( id != bigWait++ ) throw "assert";
						pos += 4;
						len -= 4;
						hasBig = true;
					}
					if( hasBig )
						log('[$cid] $bigWait/$bigCount');
					if( bigWait < bigCount || (hasBig && len < 4) ) {
						addRemain(bytes,pos,len);
						return;
					}
					while( len >= 4 ) {
						var id = bytes.getInt32(pos);
						if( recv < 0 ) {
							recv = id;
							send = id;
						} else if( id != ++recv )
							throw "assert";
						log('[$cid] << $id');
						pos += 4;
						len -= 4;
					}
					if( send < 0 )
						throw "Sending before recv";
					log('[$cid] >> $send');
					b.setInt32(0, send++);
					cl.write(b,0,4);
					if( len != 0 ) throw "assert";
				};
				cl.onDisconnect = function() {
					log('[$cid] Disconnected');
					connected--;
					if( connected == 0 ) {
						s.close();
						done = true;
					}
				}
			};
		}

		if( useMT ) {
			haxe.EventLoop.addTask(function() {
				startServer();
				for( cid in 0...10 )
					haxe.EventLoop.addTask(() -> startClient(cid));
			});
		} else {
			startServer();
			for( cid in 0...10 )
				startClient(cid);
		}
		while( !done )
			haxe.EventLoop.main.loopOnce();
		Assert.pass();
	}
	
	public function testSSL() {
		var s = new sys.net.AsyncSocket();
		var str = null, done = false;
		s.onConnect = function() {
			s.writeString("GET / HTTP/1.1\nHost: google.com\n\n");
		};
		s.onData = function(bytes,pos,len) {
			str = bytes.getString(pos, len);
			s.close();
			done = true;
		};
		s.onDisconnect = function() {
			done = true;
		};
		s.connect(new sys.net.Host("google.com"),443,true);
		while( !done )
			haxe.EventLoop.current.loopOnce();
		Assert.isTrue(str != null && str.indexOf("Content-Type") > 0);
	}
	
}
