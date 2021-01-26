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

package sys.ssl;

import sys.ssl.Lib;
import sys.ssl.Key.KeyPtr;
import sys.ssl.Certificate.CertificatePtr;
import sys.net.Socket.SocketHandle;

private class SocketInput extends haxe.io.Input {
	@:allow(sys.ssl.Socket) private var __s:Socket;

	public function new(s:Socket) {
		this.__s = s;
	}

	public override function readByte() {
		__s.handshake();
		var r = @:privateAccess __s.ssl.recvChar();
		if (r == -1)
			throw haxe.io.Error.Blocked;
		else if (r < 0)
			throw new haxe.io.Eof();
		return r;
	}

	public override function readBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		if (pos < 0 || len < 0 || ((pos + len) : UInt) > (buf.length : UInt))
			throw haxe.io.Error.OutsideBounds;
		__s.handshake();
		var r = @:privateAccess __s.ssl.recv(buf, pos, len);
		if (r == -1)
			throw haxe.io.Error.Blocked;
		else if (r <= 0)
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if (__s != null)
			__s.close();
	}
}

private class SocketOutput extends haxe.io.Output {
	@:allow(sys.ssl.Socket) private var __s:Socket;

	public function new(s:Socket) {
		this.__s = s;
	}

	public override function writeByte(c:Int) {
		__s.handshake();
		var r = @:privateAccess __s.ssl.sendChar(c);
		if (r == -1)
			throw haxe.io.Error.Blocked;
		else if (r < 0)
			throw new haxe.io.Eof();
	}

	public override function writeBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		if (pos < 0 || len < 0 || ((pos + len) : UInt) > (buf.length : UInt))
			throw haxe.io.Error.OutsideBounds;
		__s.handshake();
		var r = @:privateAccess __s.ssl.send(buf, pos, len);
		if (r == -1)
			throw haxe.io.Error.Blocked;
		else if (r < 0)
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if (__s != null)
			__s.close();
	}
}

@:coreApi @:access(sys.net.Socket)
class Socket extends sys.net.Socket {
	public static var DEFAULT_VERIFY_CERT:Null<Bool> = true;

	public static var DEFAULT_CA:Null<Certificate>;

	private var conf:Context.Config;
	private var ssl:Context;

	public var verifyCert:Null<Bool>;

	private var caCert:Null<Certificate>;
	private var hostname:String;

	private var ownCert:Null<Certificate>;
	private var ownKey:Null<Key>;
	private var altSNIContexts:Null<Array<{match:String->Bool, key:Key, cert:Certificate}>>;
	private var sniCallback:hl.Bytes->Context.SNICbResult;
	private var handshakeDone:Bool;
	private var isBlocking:Bool = true;

	private override function init():Void {
		__s = sys.net.Socket.socket_new(false);
		input = new SocketInput(this);
		output = new SocketOutput(this);
		if (DEFAULT_VERIFY_CERT && DEFAULT_CA == null) {
			try {
				DEFAULT_CA = Certificate.loadDefaults();
			} catch (e:Dynamic) {}
		}
		verifyCert = DEFAULT_VERIFY_CERT;
		caCert = DEFAULT_CA;
	}

	public override function connect(host:sys.net.Host, port:Int):Void {
		conf = buildConfig(false);
		ssl = new Context(conf);
		ssl.setSocket(__s);
		handshakeDone = false;
		if (hostname == null)
			hostname = host.host;
		if (hostname != null)
			ssl.setHostname(@:privateAccess hostname.toUtf8());
		if (!sys.net.Socket.socket_connect(__s, host.ip, port))
			throw new Sys.SysError("Failed to connect on " + host.toString() + ":" + port);
		if (isBlocking)
			handshake();
	}

	public function handshake():Void {
		if (!handshakeDone) {
			var r = ssl.handshake();
			if (r == 0)
				handshakeDone = true;
			else if (r == -1)
				throw haxe.io.Error.Blocked;
			else
				throw new haxe.io.Eof();
		}
	}

	override function setBlocking(b:Bool):Void {
		super.setBlocking(b);
		isBlocking = b;
	}

	public function setCA(cert:Certificate):Void {
		caCert = cert;
	}

	public function setHostname(name:String):Void {
		hostname = name;
	}

	public function setCertificate(cert:Certificate, key:Key):Void {
		ownCert = cert;
		ownKey = key;
	}

	public override function close():Void {
		if (ssl != null)
			ssl.close();
		if (conf != null)
			conf.close();
		if (altSNIContexts != null)
			sniCallback = null;
		sys.net.Socket.socket_close(__s);
		var input:SocketInput = cast input;
		var output:SocketOutput = cast output;
		@:privateAccess input.__s = output.__s = null;
		input.close();
		output.close();
	}

	public function addSNICertificate(cbServernameMatch:String->Bool, cert:Certificate, key:Key):Void {
		if (altSNIContexts == null)
			altSNIContexts = [];
		altSNIContexts.push({match: cbServernameMatch, cert: cert, key: key});
	}

	public override function bind(host:sys.net.Host, port:Int):Void {
		conf = buildConfig(true);

		sys.net.Socket.socket_bind(__s, host.ip, port);
	}

	public override function accept():Socket {
		var c = sys.net.Socket.socket_accept(__s);
		if(c == null)
			throw "Blocking";
		var cssl = new Context(conf);
		cssl.setSocket(c);

		var s = Type.createEmptyInstance(sys.ssl.Socket);
		s.__s = c;
		s.ssl = cssl;
		s.input = new SocketInput(s);
		s.output = new SocketOutput(s);
		s.handshakeDone = false;

		return s;
	}

	public function peerCertificate():sys.ssl.Certificate {
		var x = ssl.getPeerCertificate();
		return x == null ? null : new sys.ssl.Certificate(x);
	}

	private function buildConfig(server:Bool):Context.Config {
		var conf = new Context.Config(server);

		if (ownCert != null && ownKey != null)
			conf.setCert(@:privateAccess ownCert.__x, @:privateAccess ownKey.__k);

		if (altSNIContexts != null) {
			sniCallback = function(servername:hl.Bytes):Context.SNICbResult {
				var servername = @:privateAccess String.fromUTF8(servername);
				for (c in altSNIContexts) {
					if (c.match(servername))
						return new Context.SNICbResult(c.cert, c.key);
				}
				if (ownKey != null && ownCert != null)
					return new Context.SNICbResult(ownCert, ownKey);
				return null;
			}
			conf.setServernameCallback(sniCallback);
		}

		if (caCert != null)
			conf.setCa(caCert == null ? null : @:privateAccess caCert.__x);
		conf.setVerify(if (verifyCert) 1 else if (verifyCert == null) 2 else 0);

		return conf;
	}
}
