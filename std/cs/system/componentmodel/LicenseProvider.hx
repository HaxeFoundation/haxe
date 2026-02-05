package cs.system.componentmodel;

/** Provides the  base class for implementing a license provider. */
@:native("System.ComponentModel.LicenseProvider")
extern class LicenseProvider {
	/**
	 * When overridden in a derived class, gets a license for an instance or type of
	 * component, when given a context and whether the denial of a license throws an
	 * exception.
	 * @param context A  that specifies where you can use the licensed object.
	 * @param type A  that represents the component requesting the license.
	 * @param instance An object that is requesting the license.
	 * @param allowExceptions if a  should be thrown when the component cannot be
	 * granted a license; otherwise, .
	 * @return A valid .
	 */
	function GetLicense(context:cs.system.componentmodel.LicenseContext, type:cs.system.Type, instance:Dynamic, allowExceptions:Bool):cs.system.componentmodel.License;
}
