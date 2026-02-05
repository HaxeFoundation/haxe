package cs.system.xml.serialization;

/** Applied to a Web service client proxy, enables you to specify an assembly that contains custom-made serializers. */
@:native("System.Xml.Serialization.XmlSerializerAssemblyAttribute")
extern class XmlSerializerAssemblyAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the name of the assembly that contains serializers for a specific
	 * set of types.
	 * @return The simple, unencrypted name of the assembly.
	 */
	var AssemblyName(default, default):String;
	/**
	 * Gets or sets the location of the assembly that contains the serializers.
	 * @return A location, such as a path or URI, that points to the assembly.
	 */
	var CodeBase(default, default):String;
	@:overload(function():Void {})
	@:overload(function(assemblyName:String):Void {})
	function new(assemblyName:String, codeBase:String):Void;
}
