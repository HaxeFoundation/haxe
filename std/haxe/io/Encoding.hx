/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package haxe.io;

/**
	String binary encoding supported by Haxe I/O
**/
enum Encoding {
	UTF8;
	/**
		Output the string the way the platform represent it in memory. This is the most efficient but is platform-specific
	**/
	RawNative;
	/**
		Strict UTF-8 mode. A decoded string is guaranteed to be valid Unicode, any
		error is indicated by throwing an error during decoding. Uses a pure Haxe
		implementation, so it is slower than `UTF8` on most platforms.
	**/
	UTF8Strict;
}

/**
	This exception is raised when decoding invalid Unicode data using
	Encoding.UTF8Strict. The `byteIndex` value indicates the position in the
	`Bytes` object at which the erroneous Unicode sequence started.
**/
enum UnicodeDecodingError {
	/**
		A continuation byte was found, but a Unicode sequence was not started.
	**/
	InvalidContinuation(byteIndex:Int);
	/**
		A continuation byte was expected next.
	**/
	ExpectedContinuation(byteIndex:Int);
	/**
		An overlong Unicode sequence was found.
	**/
	Overlong(byteIndex:Int);
	/**
		The codepoint is out of Unicode range.
	**/
	OutOfRange(byteIndex:Int);
	/**
		More data was expected.
	**/
	InsufficientData(byteIndex:Int);
}
