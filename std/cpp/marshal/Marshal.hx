package cpp.marshal;

import cpp.Char;
import cpp.UInt8;
import cpp.Char16;

@:semantics(value)
@:cpp.ValueType({ namespace : [ 'cpp', 'marshal' ] })
final extern class Marshal {
	/**
	 * Returns a view to UTF8 encoded characters of the given string. The characters in the view will be null terminated.
	 * 
	 * Depending on the internal incoding of `s` this function may allocate GC bytes.
	 * 
	 * @param s String to return the view of.
	 */
	static overload function toCharView(s:String):View<Char>;

	/**
	 * Fills `buffer` with `s` encoded as UTF8 characters. The characters in the view will be null terminated.
	 * 
	 * @param s The string to encode into the buffer.
	 * @param buffer The view to write into.
	 * @returns The number of characters written into `buffer`.
	 *
	 * @throws InvalidViewLength If `buffer` does not have enough space.
	 */
	static overload function toCharView(s:String, buffer:View<Char>):Int;

	/**
	 * Returns a view to UTF16 encoded characters of the given string. The characters in the view will be null terminated.
	 * 
	 * Depending on the internal incoding of `s` this function may allocate GC bytes.
	 * 
	 * @param s String to return the view of.
	 */
	static overload function toWideCharView(s:String):View<Char16>;

	/**
	 * Fills `buffer` with `s` encoded as UTF16 characters. The characters in the view will be null terminated.
	 * 
	 * @param s The string to encode into the buffer.
	 * @param buffer The view to write into.
	 * @returns The number of characters written into `buffer`.
	 *
	 * @throws InvalidViewLength If `buffer` does not have enough space.
	 */
	static overload function toWideCharView(s:String, buffer:View<Char16>):Int;

	/**
	 * Allocates a string from the provided utf8 characters.
	 *
	 * @param buffer Buffer to characters, does not need to be null terminated.
	 */
	static overload function toString(buffer:View<Char>):String;

	/**
	 * Allocates a string from the provided utf16 characters.
	 *
	 * @param buffer Buffer to characters, does not need to be null terminated.
	 */
	static overload function toString(buffer:View<Char16>):String;

	static function read<T>(view:View<UInt8>):T;
	static function write<T>(view:View<UInt8>, value:T):Void;
}