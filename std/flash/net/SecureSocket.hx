package flash.net;

@:require(flash11) extern class SecureSocket extends Socket {
	var serverCertificate(get,never) : flash.security.X509Certificate;
	var serverCertificateStatus(get,never) : String;
	function new() : Void;
	function addBinaryChainBuildingCertificate(certificate : flash.utils.ByteArray, trusted : Bool) : Void;
	private function get_serverCertificate() : flash.security.X509Certificate;
	private function get_serverCertificateStatus() : String;
	static var isSupported(get,never) : Bool;
	private static function get_isSupported() : Bool;
}
