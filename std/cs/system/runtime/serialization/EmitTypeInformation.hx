package cs.system.runtime.serialization;

/** Specifies how often to emit type information. */
@:native("System.Runtime.Serialization.EmitTypeInformation")
extern enum abstract EmitTypeInformation(Int) {
	var Always = 1;
	var AsNeeded = 0;
	var Never = 2;
}
