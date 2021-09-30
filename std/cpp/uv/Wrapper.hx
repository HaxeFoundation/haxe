package cpp.uv;

/**
	Base class for Haxe objects wrapping libuv entities.
**/
abstract class Wrapper {
	@:allow(cpp.uv)
	var uv:RawPointer<cpp.Void>;

// static var cnt = 0;

	function new() {
		setupUvData();
		cpp.vm.Gc.setFinalizer(this, Function.fromStaticFunction(finalizer));

		// cnt++;
		// untyped __cpp__('printf("[W]rappers created: %d\\n", {0})', cnt);
	}

	abstract function setupUvData():Void;

	function finalize() {
		Stdlib.free(Pointer.fromRaw(uv));
	}

	static function finalizer(wrapper:Wrapper) {
		// cnt--;
		// untyped __cpp__('printf("[W]rappers destroyed: %d\\n", {0})', cnt);

		wrapper.finalize();
	}
}