package flash.security;

extern class CertificateStatus {
	function new() : Void;
	static final EXPIRED : String;
	static final INVALID : String;
	static final INVALID_CHAIN : String;
	static final NOT_YET_VALID : String;
	static final PRINCIPAL_MISMATCH : String;
	static final REVOKED : String;
	static final TRUSTED : String;
	static final UNKNOWN : String;
	static final UNTRUSTED_SIGNERS : String;
}
