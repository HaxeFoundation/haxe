package cs.system.security.cryptography.x509certificates;

/** Represents an X509 extension. */
@:native("System.Security.Cryptography.X509Certificates.X509Extension")
extern class X509Extension extends cs.system.security.cryptography.AsnEncodedData {
	/**
	 * Gets a Boolean value indicating whether the extension is critical.
	 * @return if the extension is critical; otherwise, .
	 */
	var Critical(default, default):Bool;
	@:overload(function(encodedExtension:cs.system.security.cryptography.AsnEncodedData, critical:Bool):Void {})
	@:overload(function(oid:cs.system.security.cryptography.Oid, rawData:cs.NativeArray<cs.UInt8>, critical:Bool):Void {})
	function new(oid:String, rawData:cs.NativeArray<cs.UInt8>, critical:Bool):Void;
	/**
	 * Copies the extension properties of the specified  object.
	 * @param asnEncodedData The  to be copied.
	 */
	function CopyFrom(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void;
}
