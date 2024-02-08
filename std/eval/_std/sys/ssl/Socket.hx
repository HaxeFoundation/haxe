package sys.ssl;

import haxe.io.Bytes;
import eval.vm.NativeSocket;
import mbedtls.Config;
import mbedtls.Ssl;

private class SocketInput extends haxe.io.Input {
	@:allow(sys.ssl.Socket) private var socket:Socket;
	var readBuf:Bytes;

	public function new(s:Socket) {
		this.socket = s;
		readBuf = Bytes.alloc(1);
	}

	public override function readByte() {
		socket.handshake();
		var r = @:privateAccess socket.ssl.read(readBuf, 0, 1);
		if (r == -1)
			throw haxe.io.Error.Blocked;
		else if (r < 0)
			throw new haxe.io.Eof();
		return readBuf.get(0);
	}

	public override function readBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		if (pos < 0 || len < 0 || ((pos + len) : UInt) > (buf.length : UInt))
			throw haxe.io.Error.OutsideBounds;
		socket.handshake();
		var r = @:privateAccess socket.ssl.read(buf, pos, len);
		if (r == -1)
			throw haxe.io.Error.Blocked;
		else if (r <= 0)
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if (socket != null)
			socket.close();
	}
}

private class SocketOutput extends haxe.io.Output {
	@:allow(sys.ssl.Socket) private var socket:Socket;
	var writeBuf:Bytes;

	public function new(s:Socket) {
		this.socket = s;
		writeBuf = Bytes.alloc(1);
	}

	public override function writeByte(c:Int) {
		socket.handshake();
		writeBuf.set(0, c);
		var r = @:privateAccess socket.ssl.write(writeBuf, 0, 1);
		if (r == -1)
			throw haxe.io.Error.Blocked;
		else if (r < 0)
			throw new haxe.io.Eof();
	}

	public override function writeBytes(buf:haxe.io.Bytes, pos:Int, len:Int):Int {
		if (pos < 0 || len < 0 || ((pos + len) : UInt) > (buf.length : UInt))
			throw haxe.io.Error.OutsideBounds;
		socket.handshake();
		var r = @:privateAccess socket.ssl.write(buf, pos, len);
		if (r == -1)
			throw haxe.io.Error.Blocked;
		else if (r < 0)
			throw new haxe.io.Eof();
		return r;
	}

	public override function close() {
		super.close();
		if (socket != null)
			socket.close();
	}
}

@:coreApi
class Socket extends sys.net.Socket {
	public static var DEFAULT_VERIFY_CERT:Null<Bool> = true;

	public static var DEFAULT_CA:Null<Certificate>;

	private var conf:Config;
	private var ssl:Ssl;

	public var verifyCert:Null<Bool>;

	private var caCert:Null<Certificate>;
	private var hostname:String;

	private var handshakeDone:Bool;
	private var isBlocking:Bool = true;

	override function init(socket:NativeSocket):Void {
		this.socket = socket;
		input = new SocketInput(this);
		output = new SocketOutput(this);
		if (DEFAULT_VERIFY_CERT && DEFAULT_CA == null) {
			DEFAULT_CA = Certificate.loadDefaults();
		}
		verifyCert = DEFAULT_VERIFY_CERT;
		caCert = DEFAULT_CA;
	}

	public override function connect(host:sys.net.Host, port:Int):Void {
		conf = buildConfig(false);
		ssl = new Ssl();
		ssl.setup(conf);
		Mbedtls.setSocket(ssl, socket);
		handshakeDone = false;
		if (hostname == null)
			hostname = host.host;
		if (hostname != null)
			ssl.set_hostname(hostname);
		socket.connect(host.ip, port);
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
				throw mbedtls.Error.strerror(r);
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

	public override function close():Void {
		super.close();
		var input:SocketInput = cast input;
		var output:SocketOutput = cast output;
		@:privateAccess input.socket = output.socket = null;
		input.close();
		output.close();
	}

	public override function bind(host:sys.net.Host, port:Int):Void {
		conf = buildConfig(true);

		socket.bind(host.ip, port);
	}

	public override function accept():Socket {
		var c = socket.accept();
		var cssl = new Ssl();
		cssl.setup(conf);
		Mbedtls.setSocket(cssl, c);

		var s = Type.createEmptyInstance(sys.ssl.Socket);
		s.socket = c;
		s.ssl = cssl;
		s.input = new SocketInput(s);
		s.output = new SocketOutput(s);
		s.handshakeDone = false;

		return s;
	}

	public function addSNICertificate(cbServernameMatch:String->Bool, cert:Certificate, key:Key):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	public function peerCertificate():Certificate {
		return @:privateAccess new Certificate(ssl.get_peer_cert());
	}

	public function setCertificate(cert:Certificate, key:Key):Void {
		throw new haxe.exceptions.NotImplementedException();
	}

	private function buildConfig(server:Bool):Config {
		var conf = new Config();
		conf.defaults(server ? SSL_IS_SERVER : SSL_IS_CLIENT, SSL_TRANSPORT_STREAM, SSL_PRESET_DEFAULT);
		conf.rng(Mbedtls.getDefaultCtrDrbg());

		if (caCert != null) {
			conf.ca_chain(@:privateAccess caCert.getNative());
		}
		conf.authmode(if (verifyCert) SSL_VERIFY_REQUIRED else if (verifyCert == null) SSL_VERIFY_OPTIONAL else SSL_VERIFY_NONE);
		return conf;
	}
}
