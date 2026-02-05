package cs.system.componentmodel.design;

/** Defines GUID identifiers that correspond to the standard set of tool windows that are available in the design environment. */
@:native("System.ComponentModel.Design.StandardToolWindows")
extern class StandardToolWindows {
	/** Gets the GUID for the object browser. This field is read-only. */
	static var ObjectBrowser(default, never):cs.system.Guid;
	/** Gets the GUID for the output window. This field is read-only. */
	static var OutputWindow(default, never):cs.system.Guid;
	/** Gets the GUID for the solution explorer. This field is read-only. */
	static var ProjectExplorer(default, never):cs.system.Guid;
	/** Gets the GUID for the Properties window. This field is read-only. */
	static var PropertyBrowser(default, never):cs.system.Guid;
	/** Gets the GUID for the related links frame. This field is read-only. */
	static var RelatedLinks(default, never):cs.system.Guid;
	/** Gets the GUID for the server explorer. This field is read-only. */
	static var ServerExplorer(default, never):cs.system.Guid;
	/** Gets the GUID for the task list. This field is read-only. */
	static var TaskList(default, never):cs.system.Guid;
	/** Gets the GUID for the Toolbox. This field is read-only. */
	static var Toolbox(default, never):cs.system.Guid;
	function new():Void;
}
