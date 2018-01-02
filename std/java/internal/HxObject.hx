/*
 * Copyright (C)2005-2018 Haxe Foundation
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
package java.internal;
import java.internal.IEquatable;
import haxe.ds.Vector;

private typedef StdType = Type;

@:native('haxe.lang.HxObject')
@:keep
private class HxObject implements IHxObject
{
}

@:native('haxe.lang.IHxObject')
@:keep
interface IHxObject
{
}

@:native('haxe.lang.DynamicObject')
@:keep
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

@:keep @:native('haxe.lang.Enum') @:nativeGen
class HxEnum
{
	@:readOnly private var index(default,never):Int;

	public function new(index:Int)
	{
		untyped this.index = index;
	}

	public function getTag():String
	{
		return throw 'Not Implemented';
	}

	public function getParams():Array<{}>
	{
		return [];
	}

	public function toString():String
	{
		return getTag();
	}
}

@:keep @:native('haxe.lang.ParamEnum') @:nativeGen
private class ParamEnum extends HxEnum
{
	@:readOnly private var params(default,never):Vector<Dynamic>;

	public function new(index:Int, params:Vector<Dynamic>)
	{
		super(index);
		untyped this.params = params;
	}

	override public function getParams():Array<{}>
	{
		return params == null ? [] : cast params.toArray();
	}

	override public function toString():String
	{
		if (params == null || params.length == 0) return getTag();
		var ret = new StringBuf();
		ret.add(getTag()); ret.add("(");
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

	public function equals(obj:Dynamic)
	{
		if (obj == this) //we cannot use == as .Equals !
			return true;
		var obj:ParamEnum = Std.is(obj,ParamEnum) ? cast obj : null;
		var ret = obj != null && Std.is(obj, StdType.getEnum(cast this)) && obj.index == this.index;
		if (!ret)
			return false;
		if (obj.params == this.params)
			return true;
		var len = 0;
		if (obj.params == null || this.params == null || (len = this.params.length) != obj.params.length)
			return false;
		for (i in 0...len)
		{
			if (!StdType.enumEq(obj.params[i], this.params[i]))
				return false;
		}
		return true;
	}

	public function hashCode():Int
	{
		var h = 19;
		if (params != null) for (p in params)
		{
			h = h * 31;
			if (p != null)
				untyped h += p.hashCode();
		}
		h += index;
		return h;
	}
}
