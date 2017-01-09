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

@:native("NSString")  @:objc extern class NSStringData { }

@:objc
extern abstract NSString( NSStringData )
{
   inline function new(s:NSStringData) this = s;
   @:native("(id)") @:extern static function toObject(d:NSStringData) : NSObject return null;

   @:native("(NSString *)") @:extern static function castFromString(s:String) : NSString return null;
   @:native("String") @:extern static function castToString(s:NSStringData) : String return null;


   @:from @:extern
   static public inline function fromString(s:String):NSString return castFromString(s);


   @:to @:extern
   public inline function toString():String return castToString(this);

   @:to @:extern public inline function toNSObject():NSObject return toObject(this);
}

