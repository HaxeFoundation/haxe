package cs.system.runtime;

/** Indicates that the .NET Framework class library method to which this attribute is applied is unlikely to be affected by servicing releases, and therefore is eligible to be inlined across Native Image Generator (NGen) images. */
@:native("System.Runtime.TargetedPatchingOptOutAttribute")
extern class TargetedPatchingOptOutAttribute extends cs.system.Attribute {
	/**
	 * Gets the reason why the method to which this attribute is applied is considered
	 * to be eligible for inlining across Native Image Generator (NGen) images.
	 * @return The reason why the method is considered to be eligible for inlining
	 * across NGen images.
	 */
	var Reason(default, never):String;
	function new(reason:String):Void;
}
