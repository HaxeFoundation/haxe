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

package cs.internal;

import cs.internal.FieldLookup;
import cs.system.Type;
import haxe.ds.Vector;

private typedef StdType = std.Type;

@:keep @:native('haxe.lang.HxObject')
class HxObject implements IHxObject {
	public function __hx_deleteField(field:String, hash:Int):Bool {
		return false;
	}
}

@:keep @:native('haxe.lang.IHxObject')
interface IHxObject {}

#if core_api_serialize
@:meta(System.Serializable)
#end
@:keep @:native('haxe.lang.DynamicObject')
class DynamicObject extends HxObject {
	@:skipReflection var __hx_hashes:NativeArray<Int>;
	@:skipReflection var __hx_dynamics:NativeArray<Dynamic>;

	@:skipReflection var __hx_hashes_f:NativeArray<Int>;
	@:skipReflection var __hx_dynamics_f:NativeArray<Float>;

	@:nullSafety(Off)
	@:skipReflection var __hx_length:Int;
	@:nullSafety(Off)
	@:skipReflection var __hx_length_f:Int;
	@:nullSafety(Off)
	@:skipReflection var __hx_conflicts:FieldHashConflict;

	@:skipReflection static var __hx_toString_depth = 0;

	@:overload public function new() {
		this.__hx_hashes = new NativeArray(0);
		this.__hx_dynamics = new NativeArray(0);
		this.__hx_hashes_f = new NativeArray(0);
		this.__hx_dynamics_f = new NativeArray(0);
		@:nullSafety(Off)
		this.__hx_conflicts = null;
	}

	@:overload public function new(hashes:NativeArray<Int>, dynamics:NativeArray<Dynamic>, hashes_f:NativeArray<Int>, dynamics_f:NativeArray<Float>) {
		this.__hx_hashes = hashes;
		this.__hx_dynamics = dynamics;
		this.__hx_hashes_f = hashes_f;
		this.__hx_dynamics_f = dynamics_f;
		this.__hx_length = hashes.length;
		this.__hx_length_f = hashes_f.length;
		this.__hx_conflicts = null;
	}

	override public function __hx_deleteField(field:String, hash:Int):Bool {
		if (hash < 0) {
			return FieldLookup.deleteHashConflict(this.__hx_conflicts, hash, field);
		}

		var res = FieldLookup.findHash(hash, this.__hx_hashes, this.__hx_length);
		if (res >= 0) {
			FieldLookup.removeInt(this.__hx_hashes, this.__hx_length, res);
			FieldLookup.removeDynamic(this.__hx_dynamics, this.__hx_length, res);
			this.__hx_length--;
			return true;
		}
		res = FieldLookup.findHash(hash, this.__hx_hashes_f, this.__hx_length_f);
		if (res >= 0) {
			FieldLookup.removeInt(this.__hx_hashes_f, this.__hx_length_f, res);
			FieldLookup.removeFloat(this.__hx_dynamics_f, this.__hx_length_f, res);
			this.__hx_length_f--;
			return true;
		}
		return false;
	}

	public function __hx_getField(field:String, hash:Int, throwErrors:Bool, isCheck:Bool, handleProperties:Bool):Dynamic {
		if (hash < 0) {
			var conflict = FieldLookup.getHashConflict(this.__hx_conflicts, hash, field);
			if (conflict != null) {
				return conflict.value;
			}
		}

		var res = FieldLookup.findHash(hash, this.__hx_hashes, this.__hx_length);
		if (res >= 0) {
			return this.__hx_dynamics[res];
		}
		res = FieldLookup.findHash(hash, this.__hx_hashes_f, this.__hx_length_f);
		if (res >= 0) {
			return this.__hx_dynamics_f[res];
		}

		@:nullSafety(Off)
		return isCheck ? Runtime.undefined : null;
	}

	public function __hx_setField(field:String, hash:Int, value:Dynamic, handleProperties:Bool):Dynamic {
		if (hash < 0) {
			FieldLookup.setHashConflict(this.__hx_conflicts, hash, field, value);
			return value;
		}

		var res = FieldLookup.findHash(hash, this.__hx_hashes, this.__hx_length);
		if (res >= 0) {
			return this.__hx_dynamics[res] = value;
		} else {
			var res = FieldLookup.findHash(hash, this.__hx_hashes_f, this.__hx_length_f);
			if (res >= 0) {
				if (Std.isOfType(value, Float)) {
					return this.__hx_dynamics_f[res] = value;
				}

				FieldLookup.removeInt(this.__hx_hashes_f, this.__hx_length_f, res);
				FieldLookup.removeFloat(this.__hx_dynamics_f, this.__hx_length_f, res);
				this.__hx_length_f--;
			}
		}

		this.__hx_hashes = FieldLookup.insertInt(this.__hx_hashes, this.__hx_length, ~(res), hash);
		this.__hx_dynamics = FieldLookup.insertDynamic(this.__hx_dynamics, this.__hx_length, ~(res), value);
		this.__hx_length++;
		return value;
	}

	public function __hx_getField_f(field:String, hash:Int, throwErrors:Bool, handleProperties:Bool):Float {
		if (hash < 0) {
			var conflict = FieldLookup.getHashConflict(this.__hx_conflicts, hash, field);
			if (conflict != null) {
				return conflict.value;
			}
		}

		var res = FieldLookup.findHash(hash, this.__hx_hashes_f, this.__hx_length_f);
		if (res >= 0) {
			return this.__hx_dynamics_f[res];
		}
		res = FieldLookup.findHash(hash, this.__hx_hashes, this.__hx_length);
		if (res >= 0) {
			return this.__hx_dynamics[res];
		}

		return 0.0;
	}

	public function __hx_setField_f(field:String, hash:Int, value:Float, handleProperties:Bool):Float {
		if (hash < 0) {
			FieldLookup.setHashConflict(this.__hx_conflicts, hash, field, value);
			return value;
		}

		var res = FieldLookup.findHash(hash, this.__hx_hashes_f, this.__hx_length_f);
		if (res >= 0) {
			return this.__hx_dynamics_f[res] = value;
		} else {
			var res = FieldLookup.findHash(hash, this.__hx_hashes, this.__hx_length);
			if (res >= 0) {
				// return this.__hx_dynamics[res] = value;
				FieldLookup.removeInt(this.__hx_hashes, this.__hx_length, res);
				FieldLookup.removeDynamic(this.__hx_dynamics, this.__hx_length, res);
				this.__hx_length--;
			}
		}

		this.__hx_hashes_f = FieldLookup.insertInt(this.__hx_hashes_f, this.__hx_length_f, ~(res), hash);
		this.__hx_dynamics_f = FieldLookup.insertFloat(this.__hx_dynamics_f, this.__hx_length_f, ~(res), value);
		this.__hx_length_f++;
		return value;
	}

	public function __hx_getFields(baseArr:Array<String>):Void {
		for (i in 0...this.__hx_length) {
			baseArr.push(FieldLookup.lookupHash(this.__hx_hashes[i]));
		}
		for (i in 0...this.__hx_length_f) {
			baseArr.push(FieldLookup.lookupHash(this.__hx_hashes_f[i]));
		}
		FieldLookup.addHashConflictNames(this.__hx_conflicts, baseArr);
	}

	public function __hx_invokeField(field:String, hash:Int, dynargs:NativeArray<Dynamic>):Dynamic {
		if (field == "toString") {
			return this.toString();
		}
		var fn:Function = this.__hx_getField(field, hash, false, false, false);
		if (fn == null) {
			throw 'Cannot invoke field $field: It does not exist';
		}

		return untyped fn.__hx_invokeDynamic(dynargs);
	}

	@:skipReflection public function toString() {
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

	@:skipReflection public function __hx_toString():String {
		var ts = Reflect.field(this, "toString");
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

#if !erase_generics
@:keep @:native('haxe.lang.IGenericObject') interface IGenericObject {}

@:nativeGen @:keep @:native('haxe.lang.GenericInterface') class GenericInterface extends cs.system.Attribute {
	@:readOnly public var generic(default, never):cs.system.Type;

	public function new(generic) {
		super();
		untyped this.generic = generic;
	}
}
#end

@:keep
@:native('haxe.lang.Enum')
@:nativeGen
#if core_api_serialize
@:meta(System.Serializable)
#end
class HxEnum {
	@:readOnly var _hx_index(default, never):Int;

	@:protected function new(index:Int) {
		untyped this._hx_index = index;
	}

	public function getTag():String {
		return throw new haxe.exceptions.NotImplementedException();
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

	@:protected static function paramsGetHashCode(index:Int, params:Vector<Dynamic>):Int {
		var h:Int = 19;
		if (params != null)
			for (p in params) {
				h = h * 31;
				if (p != null)
					untyped h += p.GetHashCode();
			}
		h += index;
		return h;
	}
}
