package jvm;

@:keep
@:native('haxe.jvm.Object')
@:nativeGen
class Object {
	public function new() { }

	public function _hx_getField(name:String) {
		return Jvm.readFieldNoObject(this, name);
	}

	public function _hx_setField(name:String, value:Dynamic) {
		return Jvm.writeFieldNoObject(this, name, value);
	}
}
