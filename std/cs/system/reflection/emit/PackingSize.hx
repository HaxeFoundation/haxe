package cs.system.reflection.emit;

/** Specifies one of two factors that determine the memory alignment of fields when a type is marshaled. */
@:native("System.Reflection.Emit.PackingSize")
extern enum PackingSize {
	Size1;
	Size128;
	Size16;
	Size2;
	Size32;
	Size4;
	Size64;
	Size8;
	Unspecified;
}
