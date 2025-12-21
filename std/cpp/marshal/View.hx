package cpp.marshal;

import cpp.SizeT;
import haxe.Int64;
import haxe.ds.Vector;
import haxe.exceptions.ArgumentException;

@:semantics(value)
@:cpp.ValueType({ namespace:['cpp', 'marshal'], flags: [ StackOnly ] })
extern final class View<T> implements ArrayAccess<T> {
    final length : SizeT;
	final ptr : Pointer<T>;

	function new(ptr:Pointer<T>, length:SizeT):Void;

	/**
	 * Attempts to copy the data from the current view to the destination view.
	 * If the destination is smaller than the current view this function returns false and no data is written to the destination.
	 * @param destination Target of the copy operation.
	 */
	function tryCopyTo(destination:View<T>):Bool;

	/**
	 * Copies the the from the current view to the destination view.
	 * @param destination Target of the copy operation
	 * @throws ArgumentException If the destination is smaller than the current view.
	 */
	inline function copyTo(destination:View<T>) {
		if (tryCopyTo(destination) == false) {
			throw new ArgumentException("destination", "Not enough space in the destination view");
		}
	}

	/**
	 * Sets all items in the current view to their default value.
	 */
	function clear():Void;

	/**
	 * Sets all items in the current view to the specified value.
	 * @param value The value assigned to each item in the view.
	 */
	function fill(value:T):Void;

	/**
	 * Create a slice of the current view which starts at the specified index.
	 * @param start Zero based index to start the slice at.
	 */
	overload function slice(start:Int64):View<T>;

	/**
	 * Create a slice of the current view which starts at the specified index and runs for the specified length.
	 * @param start Zero based index to start the slice at.
	 * @param length Length of the slice.
	 */
	overload function slice(start:Int64, length:Int64):View<T>;

	/**
	 * Returns if the current view is empty.
	 */
	function isEmpty():Bool;

	/**
	 * Return a view interpreting this views content as a different type.
	 */
	function reinterpret<K>():View<K>;

	function compare(rhs:View<T>):Int;
}