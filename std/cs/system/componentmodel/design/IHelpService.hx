package cs.system.componentmodel.design;

/** Provides methods for showing Help topics and adding and removing Help keywords at design time. */
@:native("System.ComponentModel.Design.IHelpService")
extern interface IHelpService {
	/**
	 * Adds a context attribute to the document.
	 * @param name The name of the attribute to add.
	 * @param value The value of the attribute.
	 * @param keywordType The type of the keyword, from the enumeration .
	 */
	function AddContextAttribute(name:String, value:String, keywordType:cs.system.componentmodel.design.HelpKeywordType):Void;
	/** Removes all existing context attributes from the document. */
	function ClearContextAttributes():Void;
	/**
	 * Creates a local  to manage subcontexts.
	 * @param contextType The priority type of the subcontext to add.
	 * @return The newly created .
	 */
	function CreateLocalContext(contextType:cs.system.componentmodel.design.HelpContextType):cs.system.componentmodel.design.IHelpService;
	/**
	 * Removes a previously added context attribute.
	 * @param name The name of the attribute to remove.
	 * @param value The value of the attribute to remove.
	 */
	function RemoveContextAttribute(name:String, value:String):Void;
	/**
	 * Removes a context created with .
	 * @param localContext The local context  to remove.
	 */
	function RemoveLocalContext(localContext:cs.system.componentmodel.design.IHelpService):Void;
	/**
	 * Shows the Help topic that corresponds to the specified keyword.
	 * @param helpKeyword The keyword of the Help topic to display.
	 */
	function ShowHelpFromKeyword(helpKeyword:String):Void;
	/**
	 * Shows the Help topic that corresponds to the specified URL.
	 * @param helpUrl The URL of the Help topic to display.
	 */
	function ShowHelpFromUrl(helpUrl:String):Void;
}
