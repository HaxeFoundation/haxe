package scripthost;

#if cpp
@:keep class HostParent12376 {
	public function new() {}

	public function methodA() {
		return "HostParent.methodA()";
	}
}

@:keep class HostChild12376 extends HostParent12376 {
	override function methodA() {
		return "HostChild.methodA()";
	}

	public function methodB() {
		return "HostChild.methodB()";
	}

	public function methodC() {
		return "HostChild.methodC()";
	}
}
#end
