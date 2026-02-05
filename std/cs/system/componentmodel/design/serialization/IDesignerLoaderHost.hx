package cs.system.componentmodel.design.serialization;

/** Provides an interface that can extend a designer host to support loading from a serialized state. */
@:native("System.ComponentModel.Design.Serialization.IDesignerLoaderHost")
extern interface IDesignerLoaderHost extends cs.system.componentmodel.design.IDesignerHost extends cs.system.componentmodel.design.IServiceContainer extends cs.system.IServiceProvider {
	/**
	 * Ends the designer loading operation.
	 * @param baseClassName The fully qualified name of the base class of the document
	 * that this designer is designing.
	 * @param successful if the designer is successfully loaded; otherwise, .
	 * @param errorCollection A collection containing the errors encountered during
	 * load, if any. If no errors were encountered, pass either an empty collection or
	 * .
	 */
	function EndLoad(baseClassName:String, successful:Bool, errorCollection:cs.system.collections.ICollection):Void;
	/** Reloads the design document. */
	function Reload():Void;
}
