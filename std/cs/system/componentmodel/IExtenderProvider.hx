package cs.system.componentmodel;

/** Defines the interface for extending properties to other components in a container. */
@:native("System.ComponentModel.IExtenderProvider")
extern interface IExtenderProvider {
	/**
	 * Specifies whether this object can provide its extender properties to the
	 * specified object.
	 * @param extendee The  to receive the extender properties.
	 * @return if this object can provide extender properties to the specified object;
	 * otherwise, .
	 */
	function CanExtend(extendee:Dynamic):Bool;
}
