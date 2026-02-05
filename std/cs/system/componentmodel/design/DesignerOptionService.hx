package cs.system.componentmodel.design;

/** Provides a base class for getting and setting option values for a designer. */
@:native("System.ComponentModel.Design.DesignerOptionService")
extern class DesignerOptionService {
	/**
	 * Gets the options collection for this service.
	 * @return A  populated with available designer options.
	 */
	var Options(default, never):cs.system.componentmodel.design.DesignerOptionService_DesignerOptionCollection;
}
