package cs.system.componentmodel.design;

/** Represents a design-time license context that can support a license provider at design time. */
@:native("System.ComponentModel.Design.DesigntimeLicenseContext")
extern class DesigntimeLicenseContext extends cs.system.componentmodel.LicenseContext {
	function new():Void;
	/**
	 * Gets a saved license key.
	 * @param type The type of the license key.
	 * @param resourceAssembly The assembly to get the key from.
	 * @return The saved license key that matches the specified type.
	 */
	function GetSavedLicenseKey(type:cs.system.Type, resourceAssembly:cs.system.reflection.Assembly):String;
	/**
	 * Sets a saved license key.
	 * @param type The type of the license key.
	 * @param key The license key.
	 */
	function SetSavedLicenseKey(type:cs.system.Type, key:String):Void;
}
