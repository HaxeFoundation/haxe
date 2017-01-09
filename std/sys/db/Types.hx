/*
 * Copyright (C)2005-2017 Haxe Foundation
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
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SId = Null<Int>

/** int unsigned with auto increment **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SUId = Null<Int>

/** big int with auto increment **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SBigId = Null<Float>

@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SInt = Null<Int>

@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SUInt = Null<Int>

@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SBigInt = Null<Float>

/** single precision float **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SSingle = Null<Float>

/** double precision float **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SFloat = Null<Float>

/** use `tinyint(1)` to distinguish with int **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SBool = Null<Bool>

/** same as `varchar(n)` **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SString<Const> = String

/** date only, use `SDateTime` for date+time **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SDate = Date

/** mysql DateTime **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SDateTime = Date

/** mysql Timestamp **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef STimeStamp = Date

/** TinyText (up to 255 bytes) **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef STinyText = String

/** Text (up to 64KB) **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SSmallText = String

/** MediumText (up to 24MB) **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SText = String

/** Blob type (up to 64KB) **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SSmallBinary = haxe.io.Bytes

/** LongBlob type (up to 4GB) **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SLongBinary = haxe.io.Bytes

/** MediumBlob type (up to 24MB) **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SBinary = haxe.io.Bytes

/** same as binary(n) **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SBytes<Const> = haxe.io.Bytes

/** one byte signed `-128...127` **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef STinyInt = Null<Int>

/** two bytes signed `-32768...32767` **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SSmallInt = Null<Int>

/** three bytes signed `-8388608...8388607` **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SMediumInt = Null<Int>

/** one byte `0...255` **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef STinyUInt = Null<Int>

/** two bytes `0...65535` **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SSmallUInt = Null<Int>

/** three bytes `0...16777215` **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SMediumUInt = Null<Int>

// extra

/** specify that this field is nullable **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SNull<T> = Null<T>

/** specify that the integer use custom encoding **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SEncoded = Null<Int>

/** Haxe Serialized string **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SSerialized = String

/** native neko serialized bytes **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SNekoSerialized = haxe.io.Bytes

/** a set of bitflags of different enum values **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SFlags<T:EnumValue> = Null<haxe.EnumFlags<T>>

/** same as `SFlags` but will adapt the storage size to the number of flags **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SSmallFlags<T:EnumValue> = SFlags<T>;

/** allow to store any value in serialized form **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SData<T> = Null<T>

/** allow to store an enum value that does not have parameters as a simple int **/
@:deprecated("This class will be removed soon, please install the record-macros library")
typedef SEnum<E:EnumValue> = Null<E>

