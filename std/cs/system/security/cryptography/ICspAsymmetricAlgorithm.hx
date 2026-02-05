package cs.system.security.cryptography;

/** Defines methods that allow an  class to enumerate key container information, and import and export Microsoft Cryptographic API (CAPI)-compatible key blobs. */
@:native("System.Security.Cryptography.ICspAsymmetricAlgorithm")
extern interface ICspAsymmetricAlgorithm {
	/**
	 * Gets a  object that describes additional information about a cryptographic key
	 * pair.
	 * @return A  object that describes additional information about a cryptographic
	 * key pair.
	 */
	var CspKeyContainerInfo(default, never):cs.system.security.cryptography.CspKeyContainerInfo;
	/**
	 * Exports a blob that contains the key information associated with an  object.
	 * @param includePrivateParameters to include the private key; otherwise, .
	 * @return A byte array that contains the key information associated with an 
	 * object.
	 */
	function ExportCspBlob(includePrivateParameters:Bool):cs.NativeArray<cs.UInt8>;
	/**
	 * Imports a blob that represents asymmetric key information.
	 * @param rawData A byte array that represents an asymmetric key blob.
	 */
	function ImportCspBlob(rawData:cs.NativeArray<cs.UInt8>):Void;
}
