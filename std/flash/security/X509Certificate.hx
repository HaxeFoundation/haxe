package flash.security;

extern class X509Certificate {
	@:flash.property var encoded(get,never) : flash.utils.ByteArray;
	@:flash.property var issuer(get,never) : X500DistinguishedName;
	@:flash.property var issuerUniqueID(get,never) : String;
	@:flash.property var serialNumber(get,never) : String;
	@:flash.property var signatureAlgorithmOID(get,never) : String;
	@:flash.property var signatureAlgorithmParams(get,never) : flash.utils.ByteArray;
	@:flash.property var subject(get,never) : X500DistinguishedName;
	@:flash.property var subjectPublicKey(get,never) : String;
	@:flash.property var subjectPublicKeyAlgorithmOID(get,never) : String;
	@:flash.property var subjectUniqueID(get,never) : String;
	@:flash.property var validNotAfter(get,never) : Date;
	@:flash.property var validNotBefore(get,never) : Date;
	@:flash.property var version(get,never) : UInt;
	function new() : Void;
	private function get_encoded() : flash.utils.ByteArray;
	private function get_issuer() : X500DistinguishedName;
	private function get_issuerUniqueID() : String;
	private function get_serialNumber() : String;
	private function get_signatureAlgorithmOID() : String;
	private function get_signatureAlgorithmParams() : flash.utils.ByteArray;
	private function get_subject() : X500DistinguishedName;
	private function get_subjectPublicKey() : String;
	private function get_subjectPublicKeyAlgorithmOID() : String;
	private function get_subjectUniqueID() : String;
	private function get_validNotAfter() : Date;
	private function get_validNotBefore() : Date;
	private function get_version() : UInt;
}
