package cs.system.componentmodel;

/** Provides functionality required by all components. */
@:native("System.ComponentModel.IComponent")
extern interface IComponent extends cs.system.IDisposable {
	/**
	 * Gets or sets the  associated with the .
	 * @return The  object associated with the component; or , if the component does
	 * not have a site.
	 */
	var Site(default, default):cs.system.componentmodel.ISite;
}
