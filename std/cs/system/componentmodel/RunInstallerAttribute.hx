package cs.system.componentmodel;

/** Specifies whether the Visual Studio Custom Action Installer or the Installutil.exe (Installer Tool) should be invoked when the assembly is installed. */
@:native("System.ComponentModel.RunInstallerAttribute")
extern class RunInstallerAttribute extends cs.system.Attribute {
	/** Specifies the default visibility, which is . This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.RunInstallerAttribute;
	/** Specifies that the Visual Studio Custom Action Installer or the Installutil.exe (Installer Tool) should not be invoked when the assembly is installed. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.RunInstallerAttribute;
	/** Specifies that the Visual Studio Custom Action Installer or the Installutil.exe (Installer Tool) should be invoked when the assembly is installed. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.RunInstallerAttribute;
	/**
	 * Gets a value indicating whether an installer should be invoked during
	 * installation of an assembly.
	 * @return if an installer should be invoked during installation of an assembly;
	 * otherwise, .
	 */
	var RunInstaller(default, never):Bool;
	function new(runInstaller:Bool):Void;
	/**
	 * Determines whether the value of the specified  is equivalent to the current .
	 * @param obj The object to compare.
	 * @return if the specified  is equal to the current ; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Generates a hash code for the current .
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Determines if this attribute is the default.
	 * @return if the attribute is the default value for this attribute class;
	 * otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
