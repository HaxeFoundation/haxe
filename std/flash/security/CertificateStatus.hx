package flash.security;

extern class CertificateStatus {
	function new() : Void;
	static var EXPIRED : String;
	static var INVALID : String;
	static var INVALID_CHAIN : String;
	static var NOT_YET_VALID : String;
	static var PRINCIPAL_MISMATCH : String;
	static var REVOKED : String;
	static var TRUSTED : String;
	static var UNKNOWN : String;
	static var UNTRUSTED_SIGNERS : String;
}
