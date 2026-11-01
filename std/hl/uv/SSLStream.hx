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

package hl.uv;

class SSLStream extends Stream {

	var sub : Stream;
	var ssl : sys.ssl.Context;
	var conf : sys.ssl.Context.Config;
	var bio : hl.NativeArray<Dynamic>;
	var onWrite : Bool -> Void;
	var onData : hl.Bytes -> Int -> Void;
	var tmp = haxe.io.Bytes.alloc(0);
	var inputBuffer = haxe.io.Bytes.alloc(65536);
	var outputBuffer = haxe.io.Bytes.alloc(65536);
	var inputLen : Int = 0;
	var waitHandshake = true;

	public function new(sub,?cert:sys.ssl.Certificate,?key:sys.ssl.Key) {
		this.sub = sub;
		super(sub.handle);
		conf = new sys.ssl.Context.Config(cert != null);
		if( cert != null )
			conf.setCert(@:privateAccess cert.__x, @:privateAccess key.__k);
		var verify = sys.ssl.Socket.DEFAULT_VERIFY_CERT;
		var caCert = @:privateAccess sys.ssl.Socket.getDefaultCA();
		conf.setCa(@:privateAccess caCert?.__x);
		conf.setVerify(if( verify ) 1 else if ( verify == null) 2 else 0);
		bio = new hl.NativeArray<Dynamic>(3);
		bio[0] = this;
		bio[1] = staticRead;
		bio[2] = staticWrite;
		ssl = new sys.ssl.Context(conf);
		ssl.setBio(bio);
	}

	static function staticRead( s : SSLStream, buf : hl.Bytes, len : Int ) : Int {
		var avail = s.inputLen;
		if( avail == 0 )
			return -2;
		var size = len > avail ? avail : len;
		buf.blit(0, s.inputBuffer, 0, size);
		avail -= size;
		if( avail > 0 ) s.inputBuffer.blit(0, s.inputBuffer, size, avail);
		s.inputLen = avail;
		return size;
	}

	static function staticWrite( s : SSLStream, buf : hl.Bytes, len : Int ) : Int {
		var tmp = s.tmp;
		@:privateAccess {
			tmp.b = buf;
			tmp.length = len;
		};
		s.sub.write(tmp,s.onWrite,0,len);
		return len;
	}

	override function write(bytes:haxe.io.Bytes, ?onWrite:Bool->Void, pos = 0, len = -1) {
		this.onWrite = onWrite;
		var w = ssl.send(bytes,pos,len);
		if( w < 0 )
			onWrite(false);
	}

	override function readStartRaw(onData:hl.Bytes->Int->Void) {
		this.onData = onData;
	}

	public function handshake( onHandShake : Null<String> -> Void ) {
		ssl.handshake();
		sub.readStartRaw(function(bytes:hl.Bytes,len:Int) {
			if( len < 0 ) {
				if( waitHandshake )
					onHandShake("Connection closed");
				else
					onData(null,len);
				return;
			}
			@:privateAccess {
				tmp.b = bytes;
				tmp.length = len;
			};
			var pos = 0;
			while( len > 0 ) {
				var max = inputBuffer.length - inputLen;
				var size = len > max ? max : len;
				inputBuffer.blit(inputLen, tmp, pos, size);
				pos += size;
				len -= size;
				inputLen += size;
				if( waitHandshake ) {
					var r = try ssl.handshake() catch( e : Dynamic ) {
						onHandShake(Std.string(e));
						return;
					}
					if( r == 0 ) {
						waitHandshake = false;
						onHandShake(null);
					} else if( r == -1 )
						continue;
					else {
						onHandShake("Handshake error");
						break;
					}
				}
				var r = ssl.recv(outputBuffer, 0, outputBuffer.length);
				if( r == -1 )
					return;
				if( onData == null ) {
					// missing readStart. most likely an error, so let's discard data
					continue;
				}
				onData(outputBuffer, r);
			}
		});
	}

	override function listen(n:Int, onConnect:Void->Void) {
		sub.listen(n,function() {
			throw "TODO:handshake";
		});
	}

	override function close(?callb) {
		sub.close(function() {
			ssl.close();
			conf.close();
			bio = null;
			sub = null;
			ssl = null;
			conf = null;
			inputBuffer = outputBuffer = null;
			if( callb != null ) callb();
		});
	}

}
