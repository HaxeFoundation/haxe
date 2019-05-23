package jvm;

import java.NativeArray;

@:keep
@:native('haxe.jvm.Enum')
class Enum<T> extends java.lang.Enum<T> {
	@:nativeGen public function new(index:Int, name:String) {
		super(name, index);
	}

	public function _hx_getParameters() {
		return new java.NativeArray(0);
	}

	@:overload
	override public function toString() {
		var baseName = Type.getEnumConstructs(Type.getEnum(cast this))[ordinal()];
		var parameters = Type.enumParameters(cast this);
		if (parameters.length == 0) {
			return baseName;
		}
		return '$baseName(${@:privateAccess parameters.join(",")})';
	}
}
