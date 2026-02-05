package cs.system.componentmodel;

/** Provides functionality for nested containers, which logically contain zero or more other components and are owned by a parent component. */
@:native("System.ComponentModel.INestedContainer")
extern interface INestedContainer extends cs.system.componentmodel.IContainer extends cs.system.IDisposable {
	/**
	 * Gets the owning component for the nested container.
	 * @return The  that owns the nested container.
	 */
	var Owner(default, never):cs.system.componentmodel.IComponent;
}
