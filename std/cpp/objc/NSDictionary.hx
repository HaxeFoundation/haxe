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

@:native("NSDictionary<NSString *, id>") @:objc extern class DictionaryData { }

@:objc
extern abstract NSDictionary( DictionaryData )
{
   @:native("_hx_obj_to_nsdictionary") @:extern static function _hx_obj_to_nsdictionary(obj:Dynamic) : DictionaryData return null;
   @:native("_hx_nsdictionary_to_obj") @:extern static function _hx_nsdictionary_to_obj(d:DictionaryData) : Dynamic return null;


   inline function new(dict:DictionaryData) this = dict;

   @:from @:extern
   static public inline function fromDynamic(o:Dynamic):NSDictionary return new NSDictionary( _hx_obj_to_nsdictionary(o) );

   @:to @:extern
   public inline function toDynamic():Dynamic return _hx_nsdictionary_to_obj(this);

   @:to @:extern public inline function toNSObject():NSObject return cast this;

}

