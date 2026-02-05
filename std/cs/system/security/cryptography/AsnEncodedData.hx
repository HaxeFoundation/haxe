package cs.system.security.cryptography;

/** Represents Abstract Syntax Notation One (ASN.1)-encoded data. */
@:native("System.Security.Cryptography.AsnEncodedData")
extern class AsnEncodedData {
	/**
	 * Gets or sets the  value for an  object.
	 * @return An  object.
	 */
	var Oid(default, default):cs.system.security.cryptography.Oid;
	/**
	 * Gets or sets the Abstract Syntax Notation One (ASN.1)-encoded data represented
	 * in a byte array.
	 * @return A byte array that represents the Abstract Syntax Notation One
	 * (ASN.1)-encoded data.
	 */
	var RawData(default, default):cs.NativeArray<cs.UInt8>;
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void {})
	@:overload(function(oid:cs.system.security.cryptography.Oid, rawData:cs.NativeArray<cs.UInt8>):Void {})
	function new(oid:String, rawData:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Copies information from an  object.
	 * @param asnEncodedData The  object to base the new object on.
	 */
	function CopyFrom(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void;
	/**
	 * Returns a formatted version of the Abstract Syntax Notation One (ASN.1)-encoded
	 * data as a string.
	 * @param multiLine if the return string should contain carriage returns;
	 * otherwise, .
	 * @return A formatted string that represents the Abstract Syntax Notation One
	 * (ASN.1)-encoded data.
	 */
	function Format(multiLine:Bool):String;
}
