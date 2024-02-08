package jvm;

import haxe.ds.StringMap;

@:keep
@:native('haxe.jvm.DynamicObject')
@:nativeGen
class DynamicObject implements java.lang.Cloneable extends Object {
	@:private static var __hx_toString_depth = 0;

	@:private var _hx_fields:Null<StringMap<Dynamic>>;

	@:jvm.synthetic public var _hx_deletedAField:Null<Bool>;

	public function toString() {
		if (__hx_toString_depth >= 5) {
			return "...";
		}
		++__hx_toString_depth;
		_hx_initReflection();
		if (_hx_hasField("toString")) {
			--__hx_toString_depth;
			return _hx_getField("toString")();
		}
		var buf = new StringBuf();
		buf.addChar("{".code);
		var first = true;
		try {
			for (key in _hx_fields.keys()) {
				if (first)
					first = false
				else
					buf.add(", ");
				buf.add(key);
				buf.add(": ");
				buf.add(_hx_fields.get(key));
			}
		} catch (e:Dynamic) {
			--__hx_toString_depth;
			throw(e);
		}
		--__hx_toString_depth;
		buf.addChar("}".code);
		return buf.toString();
	}

	@:jvm.synthetic final public function _hx_deleteField(name:String) {
		_hx_initReflection();
		_hx_deletedAField = true;
		try {
			Jvm.writeFieldNoObject(this, name, null);
		} catch (_:Dynamic) {}
		return _hx_fields.remove(name);
	}

	@:jvm.synthetic final public function _hx_getFields() {
		_hx_initReflection();
		return [for (key in _hx_fields.keys()) key];
	}

	@:jvm.synthetic override public function _hx_getField<T>(name:String) {
		_hx_initReflection();
		return _hx_fields.get(name);
	}

	@:jvm.synthetic final public function _hx_hasField(name:String) {
		_hx_initReflection();
		return _hx_fields.exists(name);
	}

	@:jvm.synthetic override public function _hx_setField(name:String, value:Dynamic) {
		_hx_initReflection();
		_hx_fields.set(name, value);
	}

	@:jvm.synthetic final public function _hx_clone() {
		var clone:DynamicObject = (cast this : java.lang.Object).clone();
		if (_hx_fields != null) {
			clone._hx_fields = this._hx_fields.copy();
		}
		return clone;
	}

	@:jvm.synthetic final function _hx_initReflection() {
		if (_hx_fields == null) {
			_hx_fields = _hx_getKnownFields();
		}
	}

	@:jvm.synthetic function _hx_getKnownFields():StringMap<Dynamic> {
		return new StringMap();
	}
}
