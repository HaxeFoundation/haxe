package cs.system.reflection.emit;

/** Specifies one of two factors that determine the memory alignment of fields when a type is marshaled. */
@:native("System.Reflection.Emit.PackingSize")
extern enum abstract PackingSize(Int) {
	var Size1 = 1;
	var Size128 = 128;
	var Size16 = 16;
	var Size2 = 2;
	var Size32 = 32;
	var Size4 = 4;
	var Size64 = 64;
	var Size8 = 8;
	var Unspecified = 0;
}
