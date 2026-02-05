package cs.system.componentmodel;

/** Provides the  base class for all licenses. A license is granted to a specific instance of a component. */
@:native("System.ComponentModel.License")
extern class License {
	/**
	 * When overridden in a derived class, gets the license key granted to this
	 * component.
	 * @return A license key granted to this component.
	 */
	var LicenseKey(default, never):String;
	/** When overridden in a derived class, disposes of the resources used by the license. */
	function Dispose():Void;
}
