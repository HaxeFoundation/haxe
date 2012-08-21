/*
 * Copyright (c) 2005-2011, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package sys.db;

// basic types

/** int with auto increment **/
typedef SId = Int

/** int unsigned with auto increment **/
typedef SUId = Int

/** big int with auto increment **/
typedef SBigId = Float

typedef SInt = Int

typedef SUInt = Int

typedef SBigInt = Float

/** single precision float **/
typedef SSingle = Float

/** double precision float **/
typedef SFloat = Float

/** use tinyint(1) to distinguish with int **/
typedef SBool = Bool

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
typedef STinyInt = Int

/** two bytes signed [-32768...32767] **/
typedef SSmallInt = Int;

/** three bytes signed [-8388608...8388607] **/
typedef SMediumInt = Int;

/** one byte [0...255] **/
typedef STinyUInt = Int

/** two bytes [0...65535] **/
typedef SSmallUInt = Int;

/** three bytes [0...16777215] **/
typedef SMediumUInt = Int;

// extra

/** specify that this field is nullable **/
typedef SNull<T> = T

/** specify that the integer use custom encoding **/
typedef SEncoded = Int

/** haxe Serialized string **/
typedef SSerialized = String

/** native neko serialized bytes **/
typedef SNekoSerialized = haxe.io.Bytes

/** a set of bitflags of different enum values **/
typedef SFlags<T:EnumValue> = haxe.EnumFlags<T>

/** same as [SFlags] but will adapt the storage size to the number of flags **/
typedef SSmallFlags<T:EnumValue> = SFlags<T>;

/** allow to store any value in serialized form **/
typedef SData<T> = T
