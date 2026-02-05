package cs.system.diagnostics.tracing;

/** Provides the ability to create events for event tracing for Windows (ETW). */
@:native("System.Diagnostics.Tracing.EventSource")
extern class EventSource {
	/**
	 * Gets the activity ID of the current thread.
	 * @return The activity ID of the current thread.
	 */
	static var CurrentThreadActivityId(default, never):cs.system.Guid;
	/**
	 * Gets any exception that was thrown during the construction of the event source.
	 * @return The exception that was thrown during the construction of the event
	 * source, or  if no exception was thrown.
	 */
	var ConstructionException(default, never):cs.system.Exception;
	/**
	 * The unique identifier for the event source.
	 * @return A unique identifier for the event source.
	 */
	var Guid(default, never):cs.system.Guid;
	/**
	 * The friendly name of the class that is derived from the event source.
	 * @return The friendly name of the derived class.  The default is the simple name
	 * of the class.
	 */
	var Name(default, never):String;
	/**
	 * Gets the settings applied to this event source.
	 * @return The settings applied to this event source.
	 */
	var Settings(default, never):cs.system.diagnostics.tracing.EventSourceSettings;
	@:overload(function(eventSourceName:String):Void {})
	@:overload(function(eventSourceName:String, config:cs.system.diagnostics.tracing.EventSourceSettings):Void {})
	function new(eventSourceName:String, config:cs.system.diagnostics.tracing.EventSourceSettings, traits:cs.NativeArray<String>):Void;
	@:overload(function(eventSourceType:cs.system.Type, assemblyPathToIncludeInManifest:String):String {})
	/**
	 * Returns a string of the XML manifest that is associated with the current event
	 * source.
	 * @param eventSourceType The type of the event source.
	 * @param assemblyPathToIncludeInManifest The path to the assembly file (.dll) to
	 * include in the provider element of the manifest.
	 * @return The XML data string.
	 */
	static function GenerateManifest(eventSourceType:cs.system.Type, assemblyPathToIncludeInManifest:String, flags:cs.system.diagnostics.tracing.EventManifestOptions):String;
	/**
	 * Gets the unique identifier for this implementation of the event source.
	 * @param eventSourceType The type of the event source.
	 * @return A unique identifier for this event source type.
	 */
	static function GetGuid(eventSourceType:cs.system.Type):cs.system.Guid;
	/**
	 * Gets the friendly name of the event source.
	 * @param eventSourceType The type of the event source.
	 * @return The friendly name of the event source. The default is the simple name of
	 * the class.
	 */
	static function GetName(eventSourceType:cs.system.Type):String;
	/**
	 * Gets a snapshot of all the event sources for the application domain.
	 * @return An enumeration of all the event sources in the application domain.
	 */
	static function GetSources():cs.system.collections.generic.IEnumerable<cs.system.diagnostics.tracing.EventSource>;
	/**
	 * Sends a command to a specified event source.
	 * @param eventSource The event source to send the command to.
	 * @param command The event command to send.
	 * @param commandArguments The arguments for the event command.
	 */
	static function SendCommand(eventSource:cs.system.diagnostics.tracing.EventSource, command:cs.system.diagnostics.tracing.EventCommand, commandArguments:cs.system.collections.generic.IDictionary<String, String>):Void;
	@:overload(function(activityId:cs.system.Guid):Void {})
	/**
	 * Sets the activity ID on the current thread.
	 * @param activityId The current thread's new activity ID, or  to indicate that
	 * work on the current thread is not associated with any activity.
	 */
	static function SetCurrentThreadActivityId(activityId:cs.system.Guid, oldActivityThatWillContinue:cs.Ref<cs.system.Guid>):Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Gets the trait value associated with the specified key.
	 * @param key The key of the trait to get.
	 * @return The trait value associated with the specified key. If the key is not
	 * found, returns .
	 */
	function GetTrait(key:String):String;
	@:overload(function():Bool {})
	@:overload(function(level:cs.system.diagnostics.tracing.EventLevel, keywords:cs.system.diagnostics.tracing.EventKeywords):Bool {})
	/**
	 * Determines whether the current event source is enabled.
	 * @return if the current event source is enabled; otherwise, .
	 */
	function IsEnabled(level:cs.system.diagnostics.tracing.EventLevel, keywords:cs.system.diagnostics.tracing.EventKeywords, channel:cs.system.diagnostics.tracing.EventChannel):Bool;
	/**
	 * Obtains a string representation of the current event source instance.
	 * @return The name and unique identifier that identify the current event source.
	 */
	function ToString():String;
	@:overload(function(eventName:String):Void {})
	@:overload(function(eventName:String, options:cs.system.diagnostics.tracing.EventSourceOptions):Void {})
	@:overload(function<T>(eventName:String, data:T):Void {})
	@:overload(function<T>(eventName:String, options:cs.system.diagnostics.tracing.EventSourceOptions, data:T):Void {})
	@:overload(function<T>(eventName:String, options:cs.Ref<cs.system.diagnostics.tracing.EventSourceOptions>, data:cs.Ref<T>):Void {})
	/**
	 * Writes an event without fields, but with the specified name and default options.
	 * @param eventName The name of the event to write.
	 */
	function Write<T>(eventName:String, options:cs.Ref<cs.system.diagnostics.tracing.EventSourceOptions>, activityId:cs.Ref<cs.system.Guid>, relatedActivityId:cs.Ref<cs.system.Guid>, data:cs.Ref<T>):Void;
}
