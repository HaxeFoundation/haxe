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

package java.internal;

import java.internal.IEquatable;
import haxe.ds.Vector;

private typedef StdType = Type;

@:native('haxe.lang.HxObject')
@:keep
private class HxObject implements IHxObject {}

@:native('haxe.lang.IHxObject')
@:keep
interface IHxObject {}

@:native('haxe.lang.DynamicObject')
@:keep
class DynamicObject extends HxObject {
	@:skipReflection var __hx_fields:java.NativeArray<String>;
	@:skipReflection var __hx_dynamics:java.NativeArray<Dynamic>;

	@:skipReflection var __hx_fields_f:java.NativeArray<String>;
	@:skipReflection var __hx_dynamics_f:java.NativeArray<Float>;

	@:skipReflection var __hx_length:Int;
	@:skipReflection var __hx_length_f:Int;

	@:skipReflection static var __hx_toString_depth = 0;

	@:overload public function new() {
		this.__hx_fields = new java.NativeArray(0);
		this.__hx_dynamics = new java.NativeArray(0);
		this.__hx_fields_f = new java.NativeArray(0);
		this.__hx_dynamics_f = new java.NativeArray(0);
	}

	@:overload public function new(fields:NativeArray<String>, dynamics:NativeArray<Dynamic>, fields_f:NativeArray<String>, dynamics_f:NativeArray<Float>) {
		this.__hx_fields = fields;
		this.__hx_dynamics = dynamics;
		this.__hx_fields_f = fields_f;
		this.__hx_dynamics_f = dynamics_f;
		this.__hx_length = fields.length;
		this.__hx_length_f = fields_f.length;
	}

	public function __hx_deleteField(field:String):Bool {
		var res = FieldLookup.findHash(field, this.__hx_fields, this.__hx_length);
		if (res >= 0) {
			FieldLookup.removeString(this.__hx_fields, this.__hx_length, res);
			FieldLookup.removeDynamic(this.__hx_dynamics, this.__hx_length, res);
			this.__hx_length--;
			return true;
		}
		var res = FieldLookup.findHash(field, this.__hx_fields_f, this.__hx_length_f);
		if (res >= 0) {
			FieldLookup.removeString(this.__hx_fields_f, this.__hx_length_f, res);
			FieldLookup.removeFloat(this.__hx_dynamics_f, this.__hx_length_f, res);
			this.__hx_length_f--;
			return true;
		}
		return false;
	}

	public function __hx_getField(field:String, throwErrors:Bool, isCheck:Bool, handleProperties:Bool):Dynamic {
		var res = FieldLookup.findHash(field, this.__hx_fields, this.__hx_length);
		if (res >= 0) {
			return this.__hx_dynamics[res];
		}
		res = FieldLookup.findHash(field, this.__hx_fields_f, this.__hx_length_f);
		if (res >= 0) {
			return this.__hx_dynamics_f[res];
		}

		return isCheck ? Runtime.undefined : null;
	}

	public function __hx_setField(field:String, value:Dynamic, handleProperties:Bool):Dynamic {
		var res = FieldLookup.findHash(field, this.__hx_fields, this.__hx_length);
		if (res >= 0) {
			return this.__hx_dynamics[res] = value;
		} else {
			var res = FieldLookup.findHash(field, this.__hx_fields_f, this.__hx_length_f);
			if (res >= 0) {
				if (Std.isOfType(value, Float)) {
					return this.__hx_dynamics_f[res] = value;
				}

				FieldLookup.removeString(this.__hx_fields_f, this.__hx_length_f, res);
				FieldLookup.removeFloat(this.__hx_dynamics_f, this.__hx_length_f, res);
				this.__hx_length_f--;
			}
		}

		this.__hx_fields = FieldLookup.insertString(this.__hx_fields, this.__hx_length, ~(res), field);
		this.__hx_dynamics = FieldLookup.insertDynamic(this.__hx_dynamics, this.__hx_length, ~(res), value);
		this.__hx_length++;
		return value;
	}

	public function __hx_getField_f(field:String, throwErrors:Bool, handleProperties:Bool):Float {
		var res = FieldLookup.findHash(field, this.__hx_fields_f, this.__hx_length_f);
		if (res >= 0) {
			return this.__hx_dynamics_f[res];
		}
		res = FieldLookup.findHash(field, this.__hx_fields, this.__hx_length);
		if (res >= 0) {
			return this.__hx_dynamics[res];
		}

		return 0.0;
	}

	public function __hx_setField_f(field:String, value:Float, handleProperties:Bool):Float {
		var res = FieldLookup.findHash(field, this.__hx_fields_f, this.__hx_length_f);
		if (res >= 0) {
			return this.__hx_dynamics_f[res] = value;
		} else {
			var res = FieldLookup.findHash(field, this.__hx_fields, this.__hx_length);
			if (res >= 0) {
				// return this.__hx_dynamics[res] = value;
				FieldLookup.removeString(this.__hx_fields, this.__hx_length, res);
				FieldLookup.removeDynamic(this.__hx_dynamics, this.__hx_length, res);
				this.__hx_length--;
			}
		}

		this.__hx_fields_f = FieldLookup.insertString(this.__hx_fields_f, this.__hx_length_f, ~(res), field);
		this.__hx_dynamics_f = FieldLookup.insertFloat(this.__hx_dynamics_f, this.__hx_length_f, ~(res), value);
		this.__hx_length_f++;
		return value;
	}

	public function __hx_getFields(baseArr:Array<String>):Void {
		for (i in 0...this.__hx_length) {
			baseArr.push(this.__hx_fields[i]);
		}
		for (i in 0...this.__hx_length_f) {
			baseArr.push(this.__hx_fields_f[i]);
		}
	}

	public function __hx_invokeField(field:String, dynargs:NativeArray<Dynamic>):Dynamic {
		if (field == "toString") {
			return this.toString();
		}
		var fn:Function = this.__hx_getField(field, false, false, false);
		if (fn == null) {
			throw 'Cannot invoke field $field: It does not exist';
		}

		return untyped fn.__hx_invokeDynamic(dynargs);
	}

	public function toString():String {
		if (__hx_toString_depth >= 5) {
			return "...";
		}
		++__hx_toString_depth;
		try {
			var s = __hx_toString();
			--__hx_toString_depth;
			return s;
		} catch (e:Dynamic) {
			--__hx_toString_depth;
			throw(e);
		}
	}

	function __hx_toString() {
		var ts = this.__hx_getField("toString", false, false, false);
		if (ts != null)
			return ts();
		var ret = new StringBuf();
		ret.add("{");
		var first = true;
		for (f in Reflect.fields(this)) {
			if (first)
				first = false;
			else
				ret.add(",");
			ret.add(" ");
			ret.add(f);
			ret.add(" : ");
			ret.add(Reflect.field(this, f));
		}
		if (!first)
			ret.add(" ");
		ret.add("}");
		return ret.toString();
	}
}

@:keep @:native('haxe.lang.Enum') @:nativeGen
class HxEnum {
	@:readOnly private var index(default, never):Int;

	public function new(index:Int) {
		untyped this.index = index;
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
}

@:keep @:native('haxe.lang.ParamEnum') @:nativeGen
private class ParamEnum extends HxEnum {
	@:readOnly private var params(default, never):Vector<Dynamic>;

	public function new(index:Int, params:Vector<Dynamic>) {
		super(index);
		untyped this.params = params;
	}

	override public function getParams():Array<{}> {
		return params == null ? [] : cast params.toArray();
	}

	override public function toString():String {
		if (params == null || params.length == 0)
			return getTag();
		var ret = new StringBuf();
		ret.add(getTag());
		ret.add("(");
		var first = true;
		for (p in params) {
			if (first)
				first = false;
			else
				ret.add(",");
			ret.add(p);
		}
		ret.add(")");
		return ret.toString();
	}

	public function equals(obj:Dynamic) {
		if (obj == this) // we cannot use == as .Equals !
			return true;
		var obj:ParamEnum = Std.isOfType(obj, ParamEnum) ? cast obj : null;
		var ret = obj != null && Std.isOfType(obj, StdType.getEnum(cast this)) && obj.index == this.index;
		if (!ret)
			return false;
		if (obj.params == this.params)
			return true;
		var len = 0;
		if (obj.params == null || this.params == null || (len = this.params.length) != obj.params.length)
			return false;
		for (i in 0...len) {
			if (!StdType.enumEq(obj.params[i], this.params[i]))
				return false;
		}
		return true;
	}

	public function hashCode():Int {
		var h = 19;
		if (params != null)
			for (p in params) {
				h = h * 31;
				if (p != null)
					untyped h += p.hashCode();
			}
		h += index;
		return h;
	}
}
