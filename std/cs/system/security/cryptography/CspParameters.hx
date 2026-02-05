package cs.system.security.cryptography;

/** Contains parameters that are passed to the cryptographic service provider (CSP) that performs cryptographic computations. This class cannot be inherited. */
@:native("System.Security.Cryptography.CspParameters")
extern class CspParameters {
	/** Represents the key container name for . */
	var KeyContainerName:String;
	/** Specifies whether an asymmetric key is created as a signature key or an exchange key. */
	var KeyNumber:Int;
	/** Represents the provider name for . */
	var ProviderName:String;
	/** Represents the provider type code for . */
	var ProviderType:Int;
	/**
	 * Represents the flags for  that modify the behavior of the cryptographic service
	 * provider (CSP).
	 * @return An enumeration value, or a bitwise combination of enumeration values.
	 */
	var Flags(default, default):cs.system.security.cryptography.CspProviderFlags;
	/**
	 * Gets or sets a password associated with a smart card key.
	 * @return A password associated with a smart card key.
	 */
	var KeyPassword(default, default):cs.system.security.SecureString;
	/**
	 * Gets or sets a handle to the unmanaged parent window for a smart card password
	 * dialog box.
	 * @return A handle to the parent window for a smart card password dialog box.
	 */
	var ParentWindowHandle(default, default):cs.system.IntPtr;
	@:overload(function():Void {})
	@:overload(function(dwTypeIn:Int):Void {})
	@:overload(function(dwTypeIn:Int, strProviderNameIn:String):Void {})
	function new(dwTypeIn:Int, strProviderNameIn:String, strContainerNameIn:String):Void;
}
