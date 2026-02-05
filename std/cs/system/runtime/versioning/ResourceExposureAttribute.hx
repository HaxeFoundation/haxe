package cs.system.runtime.versioning;

/** Specifies the resource exposure for a member of a class. This class cannot be inherited. */
@:native("System.Runtime.Versioning.ResourceExposureAttribute")
extern class ResourceExposureAttribute extends cs.system.Attribute {
	/**
	 * Gets the resource exposure scope.
	 * @return A  object.
	 */
	var ResourceExposureLevel(default, never):cs.system.runtime.versioning.ResourceScope;
	function new(exposureLevel:cs.system.runtime.versioning.ResourceScope):Void;
}
