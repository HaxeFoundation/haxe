package cs.system.componentmodel.design;

/** Provides support for root-level designer view technologies. */
@:native("System.ComponentModel.Design.IRootDesigner")
extern interface IRootDesigner extends cs.system.componentmodel.design.IDesigner extends cs.system.IDisposable {
	/**
	 * Gets the set of technologies that this designer can support for its display.
	 * @return An array of supported  values.
	 */
	var SupportedTechnologies(default, never):cs.NativeArray<cs.system.componentmodel.design.ViewTechnology>;
	/**
	 * Gets a view object for the specified view technology.
	 * @param technology A  that indicates a particular view technology.
	 * @return An object that represents the view for this designer.
	 */
	function GetView(technology:cs.system.componentmodel.design.ViewTechnology):Dynamic;
}
