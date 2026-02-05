package cs.system.componentmodel;

/** Provides an implementation of a . The provider works in a similar fashion to the Microsoft .NET Framework standard licensing model. */
@:native("System.ComponentModel.LicFileLicenseProvider")
extern class LicFileLicenseProvider extends cs.system.componentmodel.LicenseProvider {
	function new():Void;
	/**
	 * Returns a license for the instance of the component, if one is available.
	 * @param context A  that specifies where you can use the licensed object.
	 * @param type A  that represents the component requesting the .
	 * @param instance An object that requests the .
	 * @param allowExceptions if a  should be thrown when a component cannot be granted
	 * a license; otherwise, .
	 * @return A valid . If this method cannot find a valid  or a valid  parameter, it
	 * returns .
	 */
	function GetLicense(context:cs.system.componentmodel.LicenseContext, type:cs.system.Type, instance:Dynamic, allowExceptions:Bool):cs.system.componentmodel.License;
}
