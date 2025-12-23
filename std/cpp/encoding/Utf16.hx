package cpp.encoding;

import cpp.UInt8;
import cpp.Char32;
import cpp.Reference;
import cpp.marshal.View;
import haxe.Int64;
import haxe.extern.AsVar;

@:semantics(value)
@:cpp.PointerType({ namespace : [ "cpp", "encoding" ] })
extern class Utf16 {
    /**
     * Returns if the provided string is UTF-16 encoded.
     */
    static function isEncoded(string:String):Bool;

    /**
     * Calculates the number of bytes needed to encode the given codepoint.
     */
    static overload function getByteCount(codepoint:Char32):Int;

    /**
     * Calculates the number of bytes needed to encode the given string.
     */
    static overload function getByteCount(string:String):Int64;

    /**
     * Calculates the number of characters needed to encode the given codepoint.
     */
    static overload function getCharCount(codepoint:Char32):Int;

    /**
     * Calculates the number of characters needed to encode the given string.
     */
    static overload function getCharCount(string:String):Int64;

    /**
     * Encodes all characters in the string to UTF-16 bytes. Endianness of the characters are machine specific.
     *
     * If the provided buffer is too small to fit the encoded data an exception is thrown and no data is written.
     *
     * @param string String to encode.
     * @param buffer Buffer bytes will be witten into.
     * @return Number of bytes written into the buffer.
     */
    static overload function encode(string:String, buffer:View<UInt8>):Int64;

    /**
     * Encodes the given codepoint to UTF-16 bytes. Endianness of the characters are machine specific.
     *
     * If the provided buffer is too small to fit the encoded data an exception is thrown and no data is written.
     *
     * @param codepoint Unicode codepoint to encode.
     * @param buffer Buffer bytes will be written into.
     * @return Number of bytes written into the buffer.
     */
    static overload function encode(codepoint:Char32, buffer:View<UInt8>):Int;

    /**
     * Decodes all bytes in the buffer into a string. An empty string is returned if the buffer is empty.
     */
    static function decode(buffer:View<UInt8>):String;

    /**
     * Decodes a UTF-16 encoded codepoint from the buffer. Characters are read in machine specific endianness.
     *
     * If the provided buffer is too small for the codepoint an exception is raised.
     *
     * @param codepoint The decoded codepoint is written to this variable.
     * @return Number of bytes read to decode the codepoint.
     */
    static function codepoint(buffer:View<UInt8>):Char32;
}