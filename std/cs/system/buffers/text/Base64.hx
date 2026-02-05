package cs.system.buffers.text;

/** Converts between binary data and UTF-8 encoded text that is represented in base 64. */
@:native("System.Buffers.Text.Base64")
extern class Base64 {
	/**
	 * Decodes the span of UTF-8 encoded text represented as base 64 into binary data.
	 * If the input is not a multiple of 4, it will decode as much as it can, to the
	 * closest multiple of 4.
	 * @param utf8 The input span that contains UTF-8 encoded text in base 64 that
	 * needs to be decoded.
	 * @param bytes The output span that contains the result of the operation, that is,
	 * the decoded binary data.
	 * @param bytesConsumed The number of input bytes consumed during the operation.
	 * This can be used to slice the input for subsequent calls, if necessary.
	 * @param bytesWritten The number of bytes written into the output span. This can
	 * be used to slice the output for subsequent calls, if necessary.
	 * @param isFinalBlock (default) if the input span contains the entire data to
	 * decode.  if the input span contains partial data with more data to follow.
	 * @return One of the enumeration values that indicates the status of the decoding
	 * operation.
	 */
	static function DecodeFromUtf8(utf8:cs.system.ReadOnlySpan<cs.UInt8>, bytes:cs.system.Span<cs.UInt8>, bytesConsumed:cs.Ref<Int>, bytesWritten:cs.Ref<Int>, ?isFinalBlock:Bool):cs.system.buffers.OperationStatus;
	/**
	 * Decodes the span of UTF-8 encoded text in base 64 (in-place) into binary data.
	 * The decoded binary output is smaller than the text data contained in the input
	 * (the operation deflates the data). If the input is not a multiple of 4, the
	 * method will not decode any data.
	 * @param buffer The input span that contains the base-64 text data that needs to
	 * be decoded.
	 * @param bytesWritten The number of bytes written into the buffer.
	 * @return One of the enumeration values that indicates the status of the decoding
	 * operation.
	 */
	static function DecodeFromUtf8InPlace(buffer:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):cs.system.buffers.OperationStatus;
	/**
	 * Encodes the span of binary data into UTF-8 encoded text represented as base 64.
	 * @param bytes The input span that contains binary data that needs to be encoded.
	 * @param utf8 The output span that contains the result of the operation, that is,
	 * the UTF-8 encoded text in base 64.
	 * @param bytesConsumed The number of input bytes consumed during the operation.
	 * This can be used to slice the input for subsequent calls, if necessary.
	 * @param bytesWritten The number of bytes written into the output span. This can
	 * be used to slice the output for subsequent calls, if necessary.
	 * @param isFinalBlock (the default) if the input span contains the entire data to
	 * encode.  if the input span contains partial data with more data to follow.
	 * @return One of the enumeration values that indicates the status of the encoding
	 * operation.
	 */
	static function EncodeToUtf8(bytes:cs.system.ReadOnlySpan<cs.UInt8>, utf8:cs.system.Span<cs.UInt8>, bytesConsumed:cs.Ref<Int>, bytesWritten:cs.Ref<Int>, ?isFinalBlock:Bool):cs.system.buffers.OperationStatus;
	/**
	 * Encodes the span of binary data (in-place) into UTF-8 encoded text represented
	 * as base 64. The encoded text output is larger than the binary data contained in
	 * the input (the operation inflates the data).
	 * @param buffer The input span that contains binary data that needs to be encoded.
	 * Because the method performs an in-place conversion, it needs to be large enough
	 * to store the result of the operation.
	 * @param dataLength The number of bytes of binary data contained within the buffer
	 * that needs to be encoded. This value must be smaller than the buffer length.
	 * @param bytesWritten The number of bytes written into the buffer.
	 * @return One of the enumeration values that indicates the status of the encoding
	 * operation.
	 */
	static function EncodeToUtf8InPlace(buffer:cs.system.Span<cs.UInt8>, dataLength:Int, bytesWritten:cs.Ref<Int>):cs.system.buffers.OperationStatus;
	/**
	 * Returns the maximum length (in bytes) of the result if you were to decode
	 * base-64 encoded text within a byte span of size .
	 * @param length 
	 */
	static function GetMaxDecodedFromUtf8Length(length:Int):Int;
	/**
	 * Returns the maximum length (in bytes) of the result if you were to encode binary
	 * data within a byte span of size .
	 * @param length 
	 */
	static function GetMaxEncodedToUtf8Length(length:Int):Int;
}
