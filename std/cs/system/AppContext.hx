package cs.system;

/** Provides members for setting and retrieving data about an application's context. */
@:native("System.AppContext")
extern class AppContext {
	/**
	 * Gets the pathname of the base directory that the assembly resolver uses to probe
	 * for assemblies.
	 * @return the pathname of the base directory that the assembly resolver uses to
	 * probe for assemblies.
	 */
	static var BaseDirectory(default, never):String;
	/**
	 * Gets the name of the framework version targeted by the current application.
	 * @return The name of the framework version targeted by the current application.
	 */
	static var TargetFrameworkName(default, never):String;
	/**
	 * Returns the value of the named data element assigned to the current application
	 * domain.
	 * @param name The name of the data element.
	 * @return The value of , if  identifies a named value; otherwise, .
	 */
	static function GetData(name:String):Dynamic;
	/**
	 * Sets the value of a switch.
	 * @param switchName The name of the switch.
	 * @param isEnabled The value of the switch.
	 */
	static function SetSwitch(switchName:String, isEnabled:Bool):Void;
	/**
	 * Tries to get the value of a switch.
	 * @param switchName The name of the switch.
	 * @param isEnabled When this method returns, contains the value of  if  was found,
	 * or  if  was not found. This parameter is passed uninitialized.
	 * @return if  was set and the  argument contains the value of the switch;
	 * otherwise, .
	 */
	static function TryGetSwitch(switchName:String, isEnabled:cs.Ref<Bool>):Bool;
}
