package cs.system.diagnostics;

/** Specifies that the type has a visualizer. This class cannot be inherited. */
@:native("System.Diagnostics.DebuggerVisualizerAttribute")
extern class DebuggerVisualizerAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the description of the visualizer.
	 * @return The description of the visualizer.
	 */
	var Description(default, default):String;
	/**
	 * Gets or sets the target type when the attribute is applied at the assembly
	 * level.
	 * @return The type that is the target of the visualizer.
	 */
	var Target(default, default):cs.system.Type;
	/**
	 * Gets or sets the fully qualified type name when the attribute is applied at the
	 * assembly level.
	 * @return The fully qualified type name of the target type.
	 */
	var TargetTypeName(default, default):String;
	/**
	 * Gets the fully qualified type name of the visualizer object source.
	 * @return The fully qualified type name of the visualizer object source.
	 */
	var VisualizerObjectSourceTypeName(default, never):String;
	/**
	 * Gets the fully qualified type name of the visualizer.
	 * @return The fully qualified visualizer type name.
	 */
	var VisualizerTypeName(default, never):String;
	@:overload(function(visualizerTypeName:String):Void {})
	@:overload(function(visualizer:cs.system.Type):Void {})
	@:overload(function(visualizerTypeName:String, visualizerObjectSourceTypeName:String):Void {})
	@:overload(function(visualizerTypeName:String, visualizerObjectSource:cs.system.Type):Void {})
	@:overload(function(visualizer:cs.system.Type, visualizerObjectSourceTypeName:String):Void {})
	function new(visualizer:cs.system.Type, visualizerObjectSource:cs.system.Type):Void;
}
