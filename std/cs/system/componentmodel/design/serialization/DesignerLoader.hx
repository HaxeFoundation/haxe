package cs.system.componentmodel.design.serialization;

/** Provides a basic designer loader interface that can be used to implement a custom designer loader. */
@:native("System.ComponentModel.Design.Serialization.DesignerLoader")
extern class DesignerLoader {
	/**
	 * Gets a value indicating whether the loader is currently loading a document.
	 * @return if the loader is currently loading a document; otherwise, .
	 */
	var Loading(default, never):Bool;
	/**
	 * Begins loading a designer.
	 * @param host The loader host through which this loader loads components.
	 */
	function BeginLoad(host:cs.system.componentmodel.design.serialization.IDesignerLoaderHost):Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
	/** Writes cached changes to the location that the designer was loaded from. */
	function Flush():Void;
}
