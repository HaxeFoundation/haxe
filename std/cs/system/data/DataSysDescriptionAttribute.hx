package cs.system.data;

/** Marks a property, event, or extender with a description. Visual designers can display this description when referencing the member. */
@:native("System.Data.DataSysDescriptionAttribute")
extern class DataSysDescriptionAttribute extends cs.system.componentmodel.DescriptionAttribute {
	function new(description:String):Void;
}
