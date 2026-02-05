package cs.system.runtime.serialization;

/** Specifies how often to emit type information. */
@:native("System.Runtime.Serialization.EmitTypeInformation")
extern enum EmitTypeInformation {
	Always;
	AsNeeded;
	Never;
}
