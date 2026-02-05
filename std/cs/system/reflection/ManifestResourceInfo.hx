package cs.system.reflection;

/** Provides access to manifest resources, which are XML files that describe application dependencies. */
@:native("System.Reflection.ManifestResourceInfo")
extern class ManifestResourceInfo {
	/**
	 * Gets the name of the file that contains the manifest resource, if it is not the
	 * same as the manifest file.
	 * @return The manifest resource's file name.
	 */
	var FileName(default, never):String;
	/**
	 * Gets the containing assembly for the manifest resource.
	 * @return The manifest resource's containing assembly.
	 */
	var ReferencedAssembly(default, never):cs.system.reflection.Assembly;
	/**
	 * Gets the manifest resource's location.
	 * @return A bitwise combination of  flags that indicates the location of the
	 * manifest resource.
	 */
	var ResourceLocation(default, never):cs.system.reflection.ResourceLocation;
	function new(containingAssembly:cs.system.reflection.Assembly, containingFileName:String, resourceLocation:cs.system.reflection.ResourceLocation):Void;
}
