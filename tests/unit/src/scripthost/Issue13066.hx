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
#end
