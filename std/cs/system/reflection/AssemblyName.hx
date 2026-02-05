package cs.system.reflection;

/** Describes an assembly's unique identity in full. */
@:native("System.Reflection.AssemblyName")
extern class AssemblyName {
	/**
	 * Gets or sets the location of the assembly as a URL.
	 * @return A string that is the URL location of the assembly.
	 */
	var CodeBase(default, default):String;
	/**
	 * Gets or sets a value that indicates what type of content the assembly contains.
	 * @return A value that indicates what type of content the assembly contains.
	 */
	var ContentType(default, default):cs.system.reflection.AssemblyContentType;
	/**
	 * Gets or sets the culture supported by the assembly.
	 * @return An object that represents the culture supported by the assembly.
	 */
	var CultureInfo(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets or sets the name of the culture associated with the assembly.
	 * @return The culture name.
	 */
	var CultureName(default, default):String;
	/**
	 * Gets the URI, including escape characters, that represents the codebase.
	 * @return A URI with escape characters.
	 */
	var EscapedCodeBase(default, never):String;
	/**
	 * Gets or sets the attributes of the assembly.
	 * @return A value that represents the attributes of the assembly.
	 */
	var Flags(default, default):cs.system.reflection.AssemblyNameFlags;
	/**
	 * Gets the full name of the assembly, also known as the display name.
	 * @return A string that is the full name of the assembly, also known as the
	 * display name.
	 */
	var FullName(default, never):String;
	/**
	 * Gets or sets the hash algorithm used by the assembly manifest.
	 * @return The hash algorithm used by the assembly manifest.
	 */
	var HashAlgorithm(default, default):cs.system.configuration.assemblies.AssemblyHashAlgorithm;
	/**
	 * Gets or sets the public and private cryptographic key pair that is used to
	 * create a strong name signature for the assembly.
	 * @return The public and private cryptographic key pair to be used to create a
	 * strong name for the assembly.
	 */
	var KeyPair(default, default):cs.system.reflection.StrongNameKeyPair;
	/**
	 * Gets or sets the simple name of the assembly. This is usually, but not
	 * necessarily, the file name of the manifest file of the assembly, minus its
	 * extension.
	 * @return The simple name of the assembly.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets a value that identifies the processor and bits-per-word of the
	 * platform targeted by an executable.
	 * @return One of the enumeration values that identifies the processor and
	 * bits-per-word of the platform targeted by an executable.
	 */
	var ProcessorArchitecture(default, default):cs.system.reflection.ProcessorArchitecture;
	/**
	 * Gets or sets the major, minor, build, and revision numbers of the assembly.
	 * @return An object that represents the major, minor, build, and revision numbers
	 * of the assembly.
	 */
	var Version(default, default):cs.system.Version;
	/**
	 * Gets or sets the information related to the assembly's compatibility with other
	 * assemblies.
	 * @return A value that represents information about the assembly's compatibility
	 * with other assemblies.
	 */
	var VersionCompatibility(default, default):cs.system.configuration.assemblies.AssemblyVersionCompatibility;
	@:overload(function():Void {})
	function new(assemblyName:String):Void;
	/**
	 * Gets the  for a given file.
	 * @param assemblyFile The path for the assembly whose  is to be returned.
	 * @return An object that represents the given assembly file.
	 */
	static function GetAssemblyName(assemblyFile:String):cs.system.reflection.AssemblyName;
	/**
	 * Returns a value indicating whether two assembly names are the same. The
	 * comparison is based on the simple assembly names.
	 * @param reference The reference assembly name.
	 * @param definition The assembly name that is compared to the reference assembly.
	 * @return if the simple assembly names are the same; otherwise, .
	 */
	static function ReferenceMatchesDefinition(reference:cs.system.reflection.AssemblyName, definition:cs.system.reflection.AssemblyName):Bool;
	/**
	 * Makes a copy of this  object.
	 * @return An object that is a copy of this  object.
	 */
	function Clone():Dynamic;
	/**
	 * Gets serialization information with all the data needed to recreate an instance
	 * of this .
	 * @param info The object to be populated with serialization information.
	 * @param context The destination context of the serialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Gets the public key of the assembly.
	 * @return A byte array that contains the public key of the assembly.
	 */
	function GetPublicKey():cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the public key token, which is the last 8 bytes of the SHA-1 hash of the
	 * public key under which the application or assembly is signed.
	 * @return A byte array that contains the public key token.
	 */
	function GetPublicKeyToken():cs.NativeArray<cs.UInt8>;
	/**
	 * Implements the  interface and is called back by the deserialization event when
	 * deserialization is complete.
	 * @param sender The source of the deserialization event.
	 */
	function OnDeserialization(sender:Dynamic):Void;
	/**
	 * Sets the public key identifying the assembly.
	 * @param publicKey A byte array containing the public key of the assembly.
	 */
	function SetPublicKey(publicKey:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Sets the public key token, which is the last 8 bytes of the SHA-1 hash of the
	 * public key under which the application or assembly is signed.
	 * @param publicKeyToken A byte array containing the public key token of the
	 * assembly.
	 */
	function SetPublicKeyToken(publicKeyToken:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Returns the full name of the assembly, also known as the display name.
	 * @return The full name of the assembly, or the class name if the full name cannot
	 * be determined.
	 */
	function ToString():String;
}
