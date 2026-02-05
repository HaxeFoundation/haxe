package cs.system.componentmodel.design;

/** Provides support for building a set of related custom designers. */
@:native("System.ComponentModel.Design.ITreeDesigner")
extern interface ITreeDesigner extends cs.system.componentmodel.design.IDesigner extends cs.system.IDisposable {
	/**
	 * Gets a collection of child designers.
	 * @return An , containing the collection of  child objects of the current
	 * designer.
	 */
	var Children(default, never):cs.system.collections.ICollection;
	/**
	 * Gets the parent designer.
	 * @return An  representing the parent designer, or  if there is no parent.
	 */
	var Parent(default, never):cs.system.componentmodel.design.IDesigner;
}
