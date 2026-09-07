import haxe.io.Bytes;
import mbedtls.Config;
import mbedtls.CtrDrbg;
import mbedtls.Entropy;
import mbedtls.Error;
import mbedtls.PkContext;
import mbedtls.Ssl;
import mbedtls.SslAuthmode;
import mbedtls.SslEndpoint;
import mbedtls.SslPreset;
import mbedtls.SslTransport;
import mbedtls.X509Crt;

class BioBridge {
	public var outgoing = new haxe.io.BytesBuffer();
	var incoming = Bytes.alloc(0);
	var incomingPos = 0;

	public function new() {}

	public function send(buf:Bytes, pos:Int, len:Int):Int {
		outgoing.addBytes(buf, pos, len);
		return len;
	}

	public function recv(buf:Bytes, pos:Int, len:Int):Int {
		final available = incoming.length - incomingPos;
		if (available <= 0)
			return Error.WANT_READ;
		final n = available > len ? len : available;
		buf.blit(pos, incoming, incomingPos, n);
		incomingPos += n;
		if (incomingPos >= incoming.length) {
			incoming = Bytes.alloc(0);
			incomingPos = 0;
		}
		return n;
	}

	function addIncoming(data:Bytes) {
		if (incomingPos > 0 && incomingPos < incoming.length) {
			incoming = incoming.sub(incomingPos, incoming.length - incomingPos);
			incomingPos = 0;
		}
		if (incoming.length == 0) {
			incoming = data;
			incomingPos = 0;
		} else {
			final merged = Bytes.alloc(incoming.length + data.length);
			merged.blit(0, incoming, 0, incoming.length);
			merged.blit(incoming.length, data, 0, data.length);
			incoming = merged;
			incomingPos = 0;
		}
	}

	public static function pump(a:BioBridge, b:BioBridge) {
		if (a.outgoing.length > 0) {
			b.addIncoming(a.outgoing.getBytes());
			a.outgoing = new haxe.io.BytesBuffer();
		}
		if (b.outgoing.length > 0) {
			a.addIncoming(b.outgoing.getBytes());
			b.outgoing = new haxe.io.BytesBuffer();
		}
	}
}

typedef Rng = {
	final drbg:CtrDrbg;
}

class Main {
	static function main() {
		testBioHandshake();
		testOwnCert();
		testAlpn();
		testMtls();
		trace("ok");
	}

	static function fixture(name:String):Bytes {
		return Bytes.ofString(sys.io.File.getContent('fixtures/$name'));
	}

	static function makeRng():Rng {
		final entropy = new Entropy();
		final drbg = new CtrDrbg();
		final r = drbg.seed(entropy, "tls-bio-test");
		if (r != 0)
			throw Error.strerror(r);
		return {drbg: drbg};
	}

	static function loadCa():X509Crt {
		final ca = new X509Crt();
		final r = ca.parse(fixture("ca.crt"));
		if (r != 0)
			throw Error.strerror(r);
		return ca;
	}

	static function loadCert(name:String):X509Crt {
		final cert = new X509Crt();
		final r = cert.parse(fixture('$name.crt'));
		if (r != 0)
			throw Error.strerror(r);
		return cert;
	}

	static function loadKey(name:String, drbg:CtrDrbg):PkContext {
		final key = new PkContext();
		final r = key.parse_key(fixture('$name.key'), null, drbg);
		if (r != 0)
			throw Error.strerror(r);
		return key;
	}

	static function runHandshake(client:Ssl, server:Ssl, clientBio:BioBridge, serverBio:BioBridge) {
		var clientDone = false;
		var serverDone = false;
		var steps = 0;
		while (!clientDone || !serverDone) {
			if (++steps > 1000)
				throw "handshake loop exceeded";
			BioBridge.pump(clientBio, serverBio);
			if (!clientDone) {
				final r = client.handshake();
				if (r == 0)
					clientDone = true;
				else if (r != Error.WANT_READ && r != Error.WANT_WRITE)
					throw 'client handshake: ${Error.strerror(r)}';
			}
			if (!serverDone) {
				final r = server.handshake();
				if (r == 0)
					serverDone = true;
				else if (r != Error.WANT_READ && r != Error.WANT_WRITE)
					throw 'server handshake: ${Error.strerror(r)}';
			}
		}
	}

	static function exchangeAppData(client:Ssl, server:Ssl, clientBio:BioBridge, serverBio:BioBridge) {
		final msg = Bytes.ofString("hello tls");
		final out = Bytes.alloc(msg.length);
		var written = 0;
		var steps = 0;
		while (written < msg.length) {
			if (++steps > 1000)
				throw "client write loop exceeded";
			BioBridge.pump(clientBio, serverBio);
			final r = client.write(msg, written, msg.length - written);
			if (r > 0)
				written += r;
			else if (r != Error.WANT_READ && r != Error.WANT_WRITE)
				throw 'client write: ${Error.strerror(r)}';
		}

		final inBuf = Bytes.alloc(msg.length);
		var read = 0;
		steps = 0;
		while (read < msg.length) {
			if (++steps > 1000)
				throw "server read loop exceeded";
			BioBridge.pump(clientBio, serverBio);
			final r = server.read(inBuf, read, msg.length - read);
			if (r > 0)
				read += r;
			else if (r != Error.WANT_READ && r != Error.WANT_WRITE)
				throw 'server read: ${Error.strerror(r)}';
		}
		if (inBuf.toString() != msg.toString())
			throw "app data mismatch";
	}

	static function makeServerConfig(?ownCert:X509Crt, ?ownKey:PkContext, ?ca:X509Crt, ?authmode:SslAuthmode,
			?alpn:Array<String>):Config {
		final rng = makeRng();
		final conf = new Config();
		conf.defaults(SslEndpoint.SSL_IS_SERVER, SslTransport.SSL_TRANSPORT_STREAM, SslPreset.SSL_PRESET_DEFAULT);
		conf.rng(rng.drbg);
		if (ownCert != null && ownKey != null)
			conf.own_cert(ownCert, ownKey);
		if (ca != null)
			conf.ca_chain(ca);
		if (authmode != null)
			conf.authmode(authmode);
		if (alpn != null)
			conf.alpn_protocols(alpn);
		return conf;
	}

	static function makeClientConfig(?ca:X509Crt, ?ownCert:X509Crt, ?ownKey:PkContext, ?authmode:SslAuthmode,
			?alpn:Array<String>):Config {
		final rng = makeRng();
		final conf = new Config();
		conf.defaults(SslEndpoint.SSL_IS_CLIENT, SslTransport.SSL_TRANSPORT_STREAM, SslPreset.SSL_PRESET_DEFAULT);
		conf.rng(rng.drbg);
		if (ca != null)
			conf.ca_chain(ca);
		if (ownCert != null && ownKey != null)
			conf.own_cert(ownCert, ownKey);
		if (authmode != null)
			conf.authmode(authmode);
		if (alpn != null)
			conf.alpn_protocols(alpn);
		return conf;
	}

	static function testBioHandshake() {
		final rng = makeRng();
		final serverCert = loadCert("server");
		final serverKey = loadKey("server", rng.drbg);
		final ca = loadCa();

		final serverConf = makeServerConfig(serverCert, serverKey);
		final clientConf = makeClientConfig(ca, null, null, SslAuthmode.SSL_VERIFY_REQUIRED);

		final clientBio = new BioBridge();
		final serverBio = new BioBridge();

		final client = new Ssl();
		if (client.setup(clientConf) != 0)
			throw "client setup failed";
		client.set_hostname("localhost");
		client.set_bio(clientBio.send, clientBio.recv);

		final server = new Ssl();
		if (server.setup(serverConf) != 0)
			throw "server setup failed";
		server.set_bio(serverBio.send, serverBio.recv);

		runHandshake(client, server, clientBio, serverBio);
		exchangeAppData(client, server, clientBio, serverBio);
	}

	static function testOwnCert() {
		final rng = makeRng();
		final serverCert = loadCert("server");
		final serverKey = loadKey("server", rng.drbg);
		final ca = loadCa();

		final serverConf = makeServerConfig(serverCert, serverKey);
		final clientConf = makeClientConfig(ca, null, null, SslAuthmode.SSL_VERIFY_REQUIRED);

		final clientBio = new BioBridge();
		final serverBio = new BioBridge();

		final client = new Ssl();
		client.setup(clientConf);
		client.set_hostname("localhost");
		client.set_bio(clientBio.send, clientBio.recv);

		final server = new Ssl();
		server.setup(serverConf);
		server.set_bio(serverBio.send, serverBio.recv);

		runHandshake(client, server, clientBio, serverBio);
	}

	static function testAlpn() {
		final rng = makeRng();
		final serverCert = loadCert("server");
		final serverKey = loadKey("server", rng.drbg);
		final ca = loadCa();
		final alpn = ["h2", "http/1.1"];

		final serverConf = makeServerConfig(serverCert, serverKey, ca, SslAuthmode.SSL_VERIFY_NONE, alpn);
		final clientConf = makeClientConfig(ca, null, null, SslAuthmode.SSL_VERIFY_REQUIRED, alpn);

		final clientBio = new BioBridge();
		final serverBio = new BioBridge();

		final client = new Ssl();
		client.setup(clientConf);
		client.set_hostname("localhost");
		client.set_bio(clientBio.send, clientBio.recv);

		final server = new Ssl();
		server.setup(serverConf);
		server.set_bio(serverBio.send, serverBio.recv);

		runHandshake(client, server, clientBio, serverBio);
		if (client.get_alpn_protocol() != "h2")
			throw 'client alpn: ${client.get_alpn_protocol()}';
		if (server.get_alpn_protocol() != "h2")
			throw 'server alpn: ${server.get_alpn_protocol()}';
	}

	static function testMtls() {
		final rng = makeRng();
		final serverCert = loadCert("server");
		final serverKey = loadKey("server", rng.drbg);
		final clientCert = loadCert("client");
		final clientKey = loadKey("client", rng.drbg);
		final ca = loadCa();

		final serverConf = makeServerConfig(serverCert, serverKey, ca, SslAuthmode.SSL_VERIFY_REQUIRED);
		final clientConf = makeClientConfig(ca, clientCert, clientKey, SslAuthmode.SSL_VERIFY_REQUIRED);

		final clientBio = new BioBridge();
		final serverBio = new BioBridge();

		final client = new Ssl();
		client.setup(clientConf);
		client.set_hostname("localhost");
		client.set_bio(clientBio.send, clientBio.recv);

		final server = new Ssl();
		server.setup(serverConf);
		server.set_bio(serverBio.send, serverBio.recv);

		runHandshake(client, server, clientBio, serverBio);
		if (server.get_peer_cert() == null)
			throw "missing peer cert";
	}
}
