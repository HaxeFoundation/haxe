package cs.system.diagnostics;

/** Specifies a description for a property or event. */
@:native("System.Diagnostics.MonitoringDescriptionAttribute")
extern class MonitoringDescriptionAttribute extends cs.system.componentmodel.DescriptionAttribute {
	function new(description:String):Void;
}
