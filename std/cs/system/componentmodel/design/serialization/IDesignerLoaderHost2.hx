package cs.system.componentmodel.design.serialization;

/** Provides an interface that extends  to specify whether errors are tolerated while loading a design document. */
@:native("System.ComponentModel.Design.Serialization.IDesignerLoaderHost2")
extern interface IDesignerLoaderHost2 extends cs.system.componentmodel.design.IDesignerHost extends cs.system.componentmodel.design.IServiceContainer extends cs.system.IServiceProvider extends cs.system.componentmodel.design.serialization.IDesignerLoaderHost {
	/**
	 * Gets or sets a value indicating whether it is possible to reload with errors.
	 * @return if the designer loader can reload the design document when errors are
	 * detected; otherwise, . The default is .
	 */
	var CanReloadWithErrors(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether errors should be ignored when  is
	 * called.
	 * @return if the designer loader will ignore errors when it reloads; otherwise, .
	 * The default is .
	 */
	var IgnoreErrorsDuringReload(default, default):Bool;
}
