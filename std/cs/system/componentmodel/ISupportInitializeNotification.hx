package cs.system.componentmodel;

/** Allows coordination of initialization for a component and its dependent properties. */
@:native("System.ComponentModel.ISupportInitializeNotification")
extern interface ISupportInitializeNotification extends cs.system.componentmodel.ISupportInitialize {
	/**
	 * Gets a value indicating whether the component is initialized.
	 * @return to indicate the component has completed initialization; otherwise, .
	 */
	var IsInitialized(default, never):Bool;
}
