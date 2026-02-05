package cs.system.componentmodel;

/** Provides the ability to retrieve the full nested name of a component. */
@:native("System.ComponentModel.INestedSite")
extern interface INestedSite extends cs.system.componentmodel.ISite extends cs.system.IServiceProvider {
	/**
	 * Gets the full name of the component in this site.
	 * @return The full name of the component in this site.
	 */
	var FullName(default, never):String;
}
