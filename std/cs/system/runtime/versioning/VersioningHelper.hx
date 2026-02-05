package cs.system.runtime.versioning;

/** Provides methods to aid developers in writing version-safe code. This class cannot be inherited. */
@:native("System.Runtime.Versioning.VersioningHelper")
extern class VersioningHelper {
	@:overload(function(name:String, from:cs.system.runtime.versioning.ResourceScope, to:cs.system.runtime.versioning.ResourceScope):String {})
	/**
	 * Returns a version-safe name based on the specified resource name and the
	 * intended resource consumption source.
	 * @param name The name of the resource.
	 * @param from The scope of the resource.
	 * @param to The desired resource consumption scope.
	 * @return A version-safe name.
	 */
	static function MakeVersionSafeName(name:String, from:cs.system.runtime.versioning.ResourceScope, to:cs.system.runtime.versioning.ResourceScope, type:cs.system.Type):String;
}
