package cs.system.runtime.compilerservices;

/** Prevents the Ildasm.exe (IL Disassembler) from disassembling an assembly. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.SuppressIldasmAttribute")
extern class SuppressIldasmAttribute extends cs.system.Attribute {
	function new():Void;
}
