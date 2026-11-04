package scripthost;

#if cpp
@:keep class HostParent12413 {
	public function new() {}

	public function methodA() {
		return "HostParent";
	}
}

@:keep class HostChild12413 extends HostParent12413 {
	override function methodA() {
		return super.methodA() + ", HostChild";
	}
}
#end
