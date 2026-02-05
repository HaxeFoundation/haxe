package cs.system.timers;

/** Sets the description that visual designers can display when referencing an event, extender, or property. */
@:native("System.Timers.TimersDescriptionAttribute")
extern class TimersDescriptionAttribute extends cs.system.componentmodel.DescriptionAttribute {
	function new(description:String):Void;
}
