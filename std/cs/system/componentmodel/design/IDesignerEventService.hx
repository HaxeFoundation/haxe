package cs.system.componentmodel.design;

/** Provides event notifications when root designers are added and removed, when a selected component changes, and when the current root designer changes. */
@:native("System.ComponentModel.Design.IDesignerEventService")
extern interface IDesignerEventService {
	/**
	 * Gets the root designer for the currently active document.
	 * @return The currently active document, or  if there is no active document.
	 */
	var ActiveDesigner(default, never):cs.system.componentmodel.design.IDesignerHost;
	/**
	 * Gets a collection of root designers for design documents that are currently
	 * active in the development environment.
	 * @return A  containing the root designers that have been created and not yet
	 * disposed.
	 */
	var Designers(default, never):cs.system.componentmodel.design.DesignerCollection;
}
