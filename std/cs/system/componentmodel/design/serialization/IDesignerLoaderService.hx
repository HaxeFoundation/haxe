package cs.system.componentmodel.design.serialization;

/** Provides an interface that can extend a designer loader to support asynchronous loading of external components. */
@:native("System.ComponentModel.Design.Serialization.IDesignerLoaderService")
extern interface IDesignerLoaderService {
	/** Registers an external component as part of the load process managed by this interface. */
	function AddLoadDependency():Void;
	/**
	 * Signals that a dependent load has finished.
	 * @param successful if the load of the designer is successful;  if errors
	 * prevented the load from finishing.
	 * @param errorCollection A collection of errors that occurred during the load, if
	 * any. If no errors occurred, pass either an empty collection or .
	 */
	function DependentLoadComplete(successful:Bool, errorCollection:cs.system.collections.ICollection):Void;
	/**
	 * Reloads the design document.
	 * @return if the reload request is accepted, or  if the loader does not allow the
	 * reload.
	 */
	function Reload():Bool;
}
