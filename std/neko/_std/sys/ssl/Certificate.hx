package sys.ssl;

@:coreApi
class Certificate {
	
	var __h : Null<Certificate>;
	var __x : Dynamic;

	@:allow(sys.ssl.Socket)
	function new( x : Dynamic, ?h: Null<Certificate> ){
		__x = x;
		__h = h;
	}

	public static function loadFile( file : String ) : Certificate {
		return new Certificate( cert_load_file( untyped file.__s ) );
	}
	
	public static function loadPath( path : String ) : Certificate {
		return new Certificate( cert_load_path( untyped path.__s ) );
	}

	public static function fromString( str : String ) : Certificate {
		return new Certificate( cert_add_pem(null, untyped str.__s) );
	}
	
	public static function loadDefaults() : Certificate {
		var x = cert_load_defaults();
		if ( x != null )
			return new Certificate( x );
		
		var defPaths = null;
		switch( Sys.systemName() ){
			case "Linux":
				defPaths = [
					"/etc/ssl/certs/ca-certificates.crt", // Debian/Ubuntu/Gentoo etc.
					"/etc/pki/tls/certs/ca-bundle.crt",   // Fedora/RHEL
					"/etc/ssl/ca-bundle.pem",             // OpenSUSE
					"/etc/pki/tls/cacert.pem",            // OpenELEC
					"/etc/ssl/certs",                     // SLES10/SLES11
					"/system/etc/security/cacerts"        // Android
				];
			case "BSD":
				defPaths = [
					"/usr/local/share/certs/ca-root-nss.crt", // FreeBSD/DragonFly
					"/etc/ssl/cert.pem",                      // OpenBSD
					"/etc/openssl/certs/ca-certificates.crt", // NetBSD	
				];
			case "Android":
				defPaths = ["/system/etc/security/cacerts"];
			default:
		}
		if( defPaths != null ){
			for( path in defPaths ){
				if( sys.FileSystem.exists(path) ){
					if( sys.FileSystem.isDirectory(path) )
						return loadPath(path);
					else
						return loadFile(path);
				}
			}
		}
		return null;
	}

	public var commonName(get,null) : Null<String>;
	public var altNames(get, null) : Array<String>;
	public var notBefore(get,null) : Date;
	public var notAfter(get,null) : Date;

	function get_commonName() : Null<String> {
		return subject("CN");
	}

	function get_altNames() : Array<String> {
		var l : Dynamic = cert_get_altnames(__x);
		var a = new Array<String>();
		while( l != null ){
			a.push(new String(l[0]));
			l = l[1];
		}
		return a;
	}
	
	public function subject( field : String ) : Null<String> {
		var s = cert_get_subject(__x, untyped field.__s);
		return s==null ? null : new String( cast s );
	}
	
	public function issuer( field : String ) : Null<String> {
		var s = cert_get_issuer(__x, untyped field.__s);
		return s==null ? null : new String( cast s );
	}

	function get_notBefore() : Date {
		var a = cert_get_notbefore( __x );
		return new Date( a[0], a[1] - 1, a[2], a[3], a[4], a[5] );
	}

	function get_notAfter() : Date {
		var a = cert_get_notafter( __x );
		return new Date( a[0], a[1] - 1, a[2], a[3], a[4], a[5] );
	}
	
	public function next() : Null<Certificate> {
		var n = cert_get_next(__x);
		return n == null ? null : new Certificate( n, __h==null ? this : __h );
	}

	public function add( pem : String ) : Void {
		cert_add_pem(__x,untyped pem.__s);
	}

	public function addDER( der : haxe.io.Bytes ) : Void {
		cert_add_der(__x,der.getData());
	}

	private static var cert_load_defaults = neko.Lib.loadLazy("ssl", "cert_load_defaults",0);
	private static var cert_load_file = neko.Lib.loadLazy("ssl", "cert_load_file",1);
	private static var cert_load_path = neko.Lib.loadLazy("ssl","cert_load_path",1);
	private static var cert_get_subject = neko.Lib.loadLazy("ssl", "cert_get_subject", 2);
	private static var cert_get_issuer = neko.Lib.loadLazy("ssl","cert_get_issuer",2);
	private static var cert_get_altnames = neko.Lib.loadLazy("ssl","cert_get_altnames",1);
	private static var cert_get_notbefore = neko.Lib.loadLazy("ssl","cert_get_notbefore",1);
	private static var cert_get_notafter = neko.Lib.loadLazy("ssl","cert_get_notafter",1);
	private static var cert_get_next = neko.Lib.loadLazy("ssl","cert_get_next",1);
	private static var cert_add_pem = neko.Lib.loadLazy("ssl","cert_add_pem",2);
	private static var cert_add_der = neko.Lib.loadLazy("ssl","cert_add_der",2);


}
