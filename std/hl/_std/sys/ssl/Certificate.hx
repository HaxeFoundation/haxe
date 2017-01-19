package sys.ssl;
import sys.ssl.Lib;

@:noDoc
typedef CertificatePtr = hl.Abstract<"hl_ssl_cert">;

@:coreApi
class Certificate {
	
	var __h : Null<Certificate>;
	var __x : CertificatePtr;

	@:allow(sys.ssl.Socket)
	function new( x : CertificatePtr, ?h: Null<Certificate> ){
		__x = x;
		__h = h;
	}

	public static function loadFile( file : String ) : Certificate {
		return new Certificate( cert_load_file( @:privateAccess file.toUtf8() ) );
	}
	
	public static function loadPath( path : String ) : Certificate {
		return new Certificate( cert_load_path( @:privateAccess path.toUtf8() ) );
	}

	public static function fromString( str : String ) : Certificate {
		return new Certificate( cert_add_pem(null, @:privateAccess str.toUtf8() ) );
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
		var a = cert_get_altnames(__x);
		return [for( e in a ) @:privateAccess String.fromUTF8(e)];
	}
	
	public function subject( field : String ) : Null<String> {
		var s = cert_get_subject(__x, @:privateAccess field.toUtf8() );
		return s==null ? null : new String( cast s );
	}
	
	public function issuer( field : String ) : Null<String> {
		var s = cert_get_issuer(__x, @:privateAccess field.toUtf8());
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
		cert_add_pem(__x, @:privateAccess pem.toUtf8());
	}

	public function addDER( der : haxe.io.Bytes ) : Void {
		cert_add_der(__x, @:privateAccess der.b, @:privateAccess der.length);
	}

	@:hlNative("ssl","cert_load_defaults") static function cert_load_defaults() : CertificatePtr { return null; }
	@:hlNative("ssl","cert_load_file") static function cert_load_file( file : hl.Bytes ) : CertificatePtr { return null; }
	@:hlNative("ssl","cert_load_path") static function cert_load_path( path : hl.Bytes ) : CertificatePtr { return null; }
	@:hlNative("ssl","cert_get_subject") static function cert_get_subject( cert : CertificatePtr, obj : hl.Bytes ) : hl.Bytes { return null; }
	@:hlNative("ssl","cert_get_issuer") static function cert_get_issuer( cert : CertificatePtr, obj : hl.Bytes ) : hl.Bytes { return null; }
	@:hlNative("ssl","cert_get_altnames") static function cert_get_altnames( cert : CertificatePtr ) : hl.NativeArray<hl.Bytes> { return null; }
	@:hlNative("ssl","cert_get_notbefore") static function cert_get_notbefore( cert : CertificatePtr ) : hl.NativeArray<Int> { return null; }
	@:hlNative("ssl","cert_get_notafter") static function cert_get_notafter( cert : CertificatePtr ) : hl.NativeArray<Int> { return null; }
	@:hlNative("ssl","cert_get_next") static function cert_get_next( cert : CertificatePtr ) : Null<CertificatePtr> { return null; }
	@:hlNative("ssl","cert_add_pem") static function cert_add_pem( cert : Null<CertificatePtr>, data : hl.Bytes ) : CertificatePtr { return null; }
	@:hlNative("ssl","cert_add_der") static function cert_add_der( cert : Null<CertificatePtr>, data : hl.Bytes, len : Int ) : CertificatePtr { return null; }
	

}
