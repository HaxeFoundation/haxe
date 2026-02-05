package cs.system.componentmodel.design.serialization;

/** Provides a stack object that can be used by a serializer to make information available to nested serializers. */
@:native("System.ComponentModel.Design.Serialization.ContextStack")
extern class ContextStack {
	/**
	 * Gets the current object on the stack.
	 * @return The current object on the stack, or  if no objects were pushed.
	 */
	var Current(default, never):Dynamic;
	@:overload(function(index0:Int):Dynamic {})
	@:native("get_Item")
	function get_Item(index0:cs.system.Type):Dynamic;
	function new():Void;
	/**
	 * Appends an object to the end of the stack, rather than pushing it onto the top
	 * of the stack.
	 * @param context A context object to append to the stack.
	 */
	function Append(context:Dynamic):Void;
	/**
	 * Removes the current object off of the stack, returning its value.
	 * @return The object removed from the stack;  if no objects are on the stack.
	 */
	function Pop():Dynamic;
	/**
	 * Pushes, or places, the specified object onto the stack.
	 * @param context The context object to push onto the stack.
	 */
	function Push(context:Dynamic):Void;
}
