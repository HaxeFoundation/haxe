package cpp.encoding;

import cpp.UInt8;
import cpp.Int64;
import cpp.marshal.View;

@:semantics(value)
@:cpp.PointerType({ namespace : [ "cpp", "encoding" ] })
extern class Ascii {
    /**
     * Returns if the provided string is ascii encoded.
     */
    static function isEncoded(string:String):Bool;

    /**
     * Encodes all characters in the string to ascii bytes.
     *
     * If the string cannot be encoded to ASCII an exception is thrown.
     * If the provided buffer is too small to fit the encoded data an exception is thrown and no data is written.
     *
     * @param string String to encode.
     * @param buffer Buffer bytes will be written into.
     * @return Number of bytes written into the buffer.
     */
    static function encode(string:String, buffer:View<UInt8>):Int64;

    /**
     * Decodes all the bytes in the buffer into a string. An empty string is returned if the buffer is empty.
     */
    static function decode(buffer:View<UInt8>):String;
}