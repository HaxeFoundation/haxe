package cs.system.componentmodel;

/** Specifies when you can use a licensed object and provides a way of obtaining additional services needed to support licenses running within its domain. */
@:native("System.ComponentModel.LicenseContext")
extern class LicenseContext {
	/**
	 * When overridden in a derived class, gets a value that specifies when you can use
	 * a license.
	 * @return One of the  values that specifies when you can use a license. The
	 * default is .
	 */
	var UsageMode(default, never):cs.system.componentmodel.LicenseUsageMode;
	function new():Void;
	/**
	 * When overridden in a derived class, returns a saved license key for the
	 * specified type, from the specified resource assembly.
	 * @param type A  that represents the type of component.
	 * @param resourceAssembly An  with the license key.
	 * @return The  for the specified type. This method returns  unless you override
	 * it.
	 */
	function GetSavedLicenseKey(type:cs.system.Type, resourceAssembly:cs.system.reflection.Assembly):String;
	/**
	 * Gets the requested service, if it is available.
	 * @param type The type of service to retrieve.
	 * @return An instance of the service, or  if the service cannot be found.
	 */
	function GetService(type:cs.system.Type):Dynamic;
	/**
	 * When overridden in a derived class, sets a license key for the specified type.
	 * @param type A  that represents the component associated with the license key.
	 * @param key The  to save for the type of component.
	 */
	function SetSavedLicenseKey(type:cs.system.Type, key:String):Void;
}
