package flash.security;

extern class CertificateStatus {
	function new() : Void;
	static var EXPIRED(default,never) : String;
	static var INVALID(default,never) : String;
	static var INVALID_CHAIN(default,never) : String;
	static var NOT_YET_VALID(default,never) : String;
	static var PRINCIPAL_MISMATCH(default,never) : String;
	static var REVOKED(default,never) : String;
	static var TRUSTED(default,never) : String;
	static var UNKNOWN(default,never) : String;
	static var UNTRUSTED_SIGNERS(default,never) : String;
}
