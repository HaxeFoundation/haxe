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
package cs.internal;
import cs.system.Type;
import haxe.ds.Vector;
private typedef StdType = std.Type;

@:keep @:native('haxe.lang.HxObject')
class HxObject implements IHxObject
{
}

@:keep @:native('haxe.lang.IHxObject')
interface IHxObject
{
}

#if core_api_serialize
@:meta(System.Serializable)
#end
@:keep @:native('haxe.lang.DynamicObject')
class DynamicObject extends HxObject implements Dynamic
{
	@:skipReflection public function toString():String
	{
		var ts = Reflect.field(this, "toString");
		if (ts != null)
			return ts();
		var ret = new StringBuf();
		ret.add("{");
		var first = true;
		for (f in Reflect.fields(this))
		{
			if( first )
				first = false;
			else
				ret.add(",");
			ret.add(" "); ret.add(f);
			ret.add(" : ");
			ret.add(Reflect.field(this, f));
		}
		if (!first) ret.add(" ");
		ret.add("}");
		return ret.toString();
	}
}

#if !erase_generics
@:keep @:native('haxe.lang.IGenericObject') interface IGenericObject
{
}

@:nativeGen @:keep @:native('haxe.lang.GenericInterface') class GenericInterface extends cs.system.Attribute
{
	@:readOnly public var generic(default,never):cs.system.Type;

	public function new(generic)
	{
		super();
		untyped this.generic = generic;
	}
}
#end

@:keep @:native('haxe.lang.Enum') @:nativeGen
#if core_api_serialize
@:meta(System.Serializable)
#end
class HxEnum {
	@:readOnly var _hx_index(default,never):Int;

	@:protected function new(index:Int) {
		untyped this._hx_index = index;
	}

	public function getTag():String {
		return throw 'Not Implemented';
	}

	public function getParams():Array<{}> {
		return [];
	}

	public function toString():String {
		return getTag();
	}

	@:protected static function paramsToString(tag:String, params:Vector<Dynamic>):String {
		var ret = new StringBuf();
		ret.add(tag);
		ret.add("(");
		var first = true;
		for (p in params)
		{
			if (first)
				first = false;
			else
				ret.add(",");
			ret.add(p);
		}
		ret.add(")");
		return ret.toString();
	}

	@:protected static function paramsGetHashCode(index:Int, params:Vector<Dynamic>):Int {
		var h:Int = 19;
		if (params != null) for (p in params)
		{
			h = h * 31;
			if (p != null)
				untyped h += p.GetHashCode();
		}
		h += index;
		return h;
	}
}
