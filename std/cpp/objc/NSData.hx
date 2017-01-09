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
package cpp.objc;

@:native("NSData") @:objc extern class NSDataData { }

@:objc
extern abstract NSData( NSDataData )
{
   @:native("_hx_value_to_objc") @:extern static function to_data(b:haxe.io.BytesData) : NSData return null;
   @:native("_hx_value_to_objc") @:extern static function to_data_data(b:haxe.io.BytesData) : NSDataData return null;
   @:native("_hx_objc_to_bytes") @:extern static function NSDataDataToBytes(d:NSDataData) : haxe.io.BytesData return null;


   inline function new(d:NSDataData) this = d;

   @:from @:extern
   static public inline function fromBytesData(d:haxe.io.BytesData):NSData return new NSData( to_data_data(d) );

   @:from @:extern
   static public inline function fromBytes(d:haxe.io.Bytes):NSData return new NSData( to_data_data(d.getData()) );

   @:to @:extern
   public inline function toBytesData():haxe.io.BytesData return NSDataDataToBytes(this);

   @:to @:extern
   public inline function toBytes():haxe.io.Bytes return haxe.io.Bytes.ofData(NSDataDataToBytes(this));

   @:to @:extern public inline function toNSObject():NSObject return cast this;

}

