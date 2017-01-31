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

@:native("id")  @:objc extern class NSObjectData { }

@:objc
extern abstract NSObject( NSObjectData )
{
   @:native("_hx_value_to_objc") @:extern static function _hx_value_to_objc(obj:Dynamic) : NSObject return null;
   @:native("_hx_objc_to_dynamic") @:extern static function _hx_objc_to_dynamic(d:NSObjectData) : Dynamic return null;


   inline function new(d:NSObjectData) this = d;


   @:from @:extern
   static public inline function fromHaxe(d:Dynamic):NSObject return _hx_value_to_objc(d);


   @:to @:extern
   public inline function toHaxe():Dynamic return _hx_objc_to_dynamic(this);
}


