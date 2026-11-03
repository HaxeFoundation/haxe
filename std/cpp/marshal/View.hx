package cpp.marshal;

import haxe.Int64;
import haxe.ds.Vector;
import haxe.exceptions.ArgumentException;

@:semantics(value)
@:cpp.ValueType({ namespace:['cpp', 'marshal'], flags: [ StackOnly ] })
extern final class View<T> implements ArrayAccess<T> {
    final length : Int64;
	final ptr : Pointer<T>;

	function new(ptr:Pointer<T>, length:Int64):Void;

	/**
	 * Attempts to copy the data from the current view to the destination view.
	 * If the destination is smaller than the current view this function returns false and no data is written to the destination.
	 * @param destination Target of the copy operation.
	 */
	function tryCopyTo(destination:View<T>):Bool;

	/**
	 * Copies the the from the current view to the destination view.
	 * If the destination is smaller than the current view this function raises an exception and no data is written to the destination.
	 * @param destination Target of the copy operation
	 */
	function copyTo(destination:View<T>):Void;

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

	/**
	 * Returns 0 if the contents of the two views are identical.
	 * 
	 * Returns a negative value if the length of this view is less than the length of other, or a positive value if the length of this view is greater than the length of other.
	 *
	 * In case of equal lengths, returns a negative value if the first different value in other is greater than the corresponding value in this view. Otherwise, returns a positive value.
	 */
	function compare(other:View<T>):Int;
}