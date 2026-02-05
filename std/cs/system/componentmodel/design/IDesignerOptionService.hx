package cs.system.componentmodel.design;

/** Provides access to the designer options located on the Tools menu under the Options command in the Visual Studio development environment. */
@:native("System.ComponentModel.Design.IDesignerOptionService")
extern interface IDesignerOptionService {
	/**
	 * Gets the value of the specified Windows Forms Designer option.
	 * @param pageName The name of the page that defines the option.
	 * @param valueName The name of the option property.
	 * @return The value of the specified option.
	 */
	function GetOptionValue(pageName:String, valueName:String):Dynamic;
	/**
	 * Sets the value of the specified Windows Forms Designer option.
	 * @param pageName The name of the page that defines the option.
	 * @param valueName The name of the option property.
	 * @param value The new value.
	 */
	function SetOptionValue(pageName:String, valueName:String, value:Dynamic):Void;
}
