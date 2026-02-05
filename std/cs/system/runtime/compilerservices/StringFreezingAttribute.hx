package cs.system.runtime.compilerservices;

/** Deprecated. Freezes a string literal when creating native images using the Ngen.exe (Native Image Generator). This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.StringFreezingAttribute")
extern class StringFreezingAttribute extends cs.system.Attribute {
	function new():Void;
}
