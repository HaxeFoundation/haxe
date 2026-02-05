package cs.system.runtime.compilerservices;

/** A class whose static  method checks whether a specified feature is supported by the common language runtime. */
@:native("System.Runtime.CompilerServices.RuntimeFeature")
extern class RuntimeFeature {
	static var DefaultImplementationsOfInterfaces(default, never):String;
	/** Gets the name of the portable PDB feature. */
	static var PortablePdb(default, never):String;
	/**
	 * Gets a value that indicates whether the runtime compiles dynamic code.
	 * @return if the runtime compiles dynamic code;  if it doesn't compile dynamic
	 * code or doesn't know about this property.
	 */
	static var IsDynamicCodeCompiled(default, never):Bool;
	/**
	 * Gets a value that determines whether the runtime supports dynamic code.
	 * @return if the runtime supports dynamic code;  if it either doesn't support
	 * dynamic code or doesn't know about this property.
	 */
	static var IsDynamicCodeSupported(default, never):Bool;
	/**
	 * Determines whether a specified feature is supported by the common language
	 * runtime.
	 * @param feature The name of the feature.
	 * @return if  is supported; otherwise, .
	 */
	static function IsSupported(feature:String):Bool;
}
