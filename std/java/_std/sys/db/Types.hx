/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package sys.db;

// basic types

/** int with auto increment **/
@:noPackageRestrict
typedef SId = Null<Int>

/** int unsigned with auto increment **/
typedef SUId = Null<Int>

/** big int with auto increment **/
typedef SBigId = Null<Float>

typedef SInt = Null<Int>

typedef SUInt = Null<Int>

typedef SBigInt = Null<Float>

/** single precision float **/
typedef SSingle = Null<Float>

/** double precision float **/
typedef SFloat = Null<Float>

/** use tinyint(1) to distinguish with int **/
typedef SBool = Null<Bool>

/** same as varchar(n) **/
typedef SString<Const> = String

/** date only, use SDateTime for date+time **/
typedef SDate = Date

/** mysql DateTime **/
typedef SDateTime = Date

/** mysql Timestamp **/
typedef STimeStamp = Date

/** TinyText (up to 255 bytes) **/
typedef STinyText = String

/** Text (up to 64KB) **/
typedef SSmallText = String

/** MediumText (up to 24MB) **/
typedef SText = String

/** Blob type (up to 64KB) **/
typedef SSmallBinary = haxe.io.Bytes

/** LongBlob type (up to 4GB) **/
typedef SLongBinary = haxe.io.Bytes

/** MediumBlob type (up to 24MB) **/
typedef SBinary = haxe.io.Bytes

/** same as binary(n) **/
typedef SBytes<Const> = haxe.io.Bytes

/** one byte signed [-128...127] **/
typedef STinyInt = Null<Int>

/** two bytes signed [-32768...32767] **/
typedef SSmallInt = Null<Int>;

/** three bytes signed [-8388608...8388607] **/
typedef SMediumInt = Null<Int>;

/** one byte [0...255] **/
typedef STinyUInt = Null<Int>

/** two bytes [0...65535] **/
typedef SSmallUInt = Null<Int>;

/** three bytes [0...16777215] **/
typedef SMediumUInt = Null<Int>;

// extra

/** specify that this field is nullable **/
typedef SNull<T> = Null<T>

/** specify that the integer use custom encoding **/
typedef SEncoded = Null<Int>

/** haxe Serialized string **/
typedef SSerialized = String

/** native neko serialized bytes **/
typedef SNekoSerialized = haxe.io.Bytes

/** a set of bitflags of different enum values **/
typedef SFlags<T:EnumValue> = Null<haxe.EnumFlags<T>>

/** same as [SFlags] but will adapt the storage size to the number of flags **/
typedef SSmallFlags<T:EnumValue> = SFlags<T>;

/** allow to store any value in serialized form **/
typedef SData<T> = Null<T>

/** allow to store an enum value that does not have parameters as a simple int **/
typedef SEnum<E:EnumValue> = Null<E>

