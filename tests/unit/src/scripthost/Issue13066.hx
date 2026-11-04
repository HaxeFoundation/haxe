package scripthost;

#if cpp
@:keep class HostParent13066 {
	public function new() {}

	public function toString(indent:Int = 0) {
		return 'HostParent.toString($indent)';
	}
}

@:keep class HostChild13066 extends HostParent13066 {}

@:keep class HostOverride13066 extends HostParent13066 {
	override public function toString(indent:Int = 0) {
		return 'HostOverride.toString($indent)';
	}
}

// A static toString() without arguments does not compile: it would hide hx::Object's virtual one
@:keep class HostStaticParent13066 {
	public function new() {}

	public static function toString(indent:Int = 0) {
		return 'HostStaticParent.toString($indent)';
	}
}

@:keep class HostStaticChild13066 extends HostStaticParent13066 {}
#end
