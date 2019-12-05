package sys.ssl;

import mbedtls.Entropy;
import mbedtls.CtrDrbg;

class Mbedtls {
	static public var entropy = new Entropy();
	static public var ctr = {
		var v = new CtrDrbg();
		v.seed(entropy);
		v;
	}

	static public function loadDefaultCertificates(certificate:mbedtls.Certificate) {
		if (loadDefaults(certificate) != 0) {
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

	extern static function loadDefaults(certificate:mbedtls.Certificate):Int;
}
