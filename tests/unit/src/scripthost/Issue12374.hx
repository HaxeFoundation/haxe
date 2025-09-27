package scripthost;

#if cpp
@:keep class HostParent {
	public function new() {}

	function toString() {
		return "HostParent.toString()";
	}

	public function methodA() {
		return 'HostParent.methodA()';
	}

	public function methodB() {
		return 'HostParent.methodB()';
	}
}
#end
