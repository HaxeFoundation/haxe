package sys.ssl;

import eval.vm.NativeSocket;
import mbedtls.Ssl;
import mbedtls.Entropy;
import mbedtls.CtrDrbg;
import mbedtls.X509Crt;

class Mbedtls {
	static var entropy:Null<Entropy>;
	static var ctr:Null<CtrDrbg>;

	static public function getDefaultEntropy() {
		if (entropy == null) {
			entropy = new Entropy();
		}
		return entropy;
	}

	static public function getDefaultCtrDrbg() {
		if (ctr == null) {
			ctr = new CtrDrbg();
			ctr.seed(getDefaultEntropy());
		}
		return ctr;
	}

	static public function loadDefaultCertificates(certificate:X509Crt) {
		if (loadDefaults(certificate) == 0) {
			return;
		}
		var defPaths = switch (Sys.systemName()) {
			case "Linux":
				[
					"/etc/ssl/certs/ca-certificates.crt", // Debian/Ubuntu/Gentoo etc.
					"/etc/pki/tls/certs/ca-bundle.crt", // Fedora/RHEL
					"/etc/ssl/ca-bundle.pem", // OpenSUSE
					"/etc/pki/tls/cacert.pem", // OpenELEC
					"/etc/ssl/certs", // SLES10/SLES11
					"/system/etc/security/cacerts" // Android
				];
			case "BSD":
				[
					"/usr/local/share/certs/ca-root-nss.crt", // FreeBSD/DragonFly
					"/etc/ssl/cert.pem", // OpenBSD
					"/etc/openssl/certs/ca-certificates.crt", // NetBSD
				];
			case "Android":
				["/system/etc/security/cacerts"];
			default:
				[];
		}
		for (path in defPaths) {
			if (sys.FileSystem.exists(path)) {
				if (sys.FileSystem.isDirectory(path))
					certificate.parse_path(path);
				else
					certificate.parse_file(path);
			}
		}
	}

	extern static public function setSocket(ssl:Ssl, socket:NativeSocket):Int;

	extern static function loadDefaults(certificate:X509Crt):Int;
}
