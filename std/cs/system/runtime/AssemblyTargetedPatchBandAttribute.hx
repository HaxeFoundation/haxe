package cs.system.runtime;

/** Specifies patch band information for targeted patching of the .NET Framework. */
@:native("System.Runtime.AssemblyTargetedPatchBandAttribute")
extern class AssemblyTargetedPatchBandAttribute extends cs.system.Attribute {
	/**
	 * Gets the patch band.
	 * @return The patch band information.
	 */
	var TargetedPatchBand(default, never):String;
	function new(targetedPatchBand:String):Void;
}
