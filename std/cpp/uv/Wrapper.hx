package cpp.uv;

/**
	Base class for Haxe objects wrapping libuv entities.
**/
@:allow(cpp.uv)
abstract class Wrapper {
	var uv:RawPointer<cpp.Void>;
	final loop:Loop;

static var cnt = 0;

	function new(loop:Loop) {
		this.loop = loop;
		setupUvData();
		cpp.vm.Gc.setFinalizer(this, Function.fromStaticFunction(finalizer));

		cnt++;
		untyped __cpp__('printf("[W]rappers created: %d\\n", {0})', cnt);
	}

	abstract function setupUvData():Void;

	/**
	 * Should be called when this object should be protected from GC.
	 */
	inline function referenceFromLoop() {
		loop.addWrapper(this);
	}

	/**
	 * Should be called when it's no longer needed to protect this object from GC.
	 */
	inline function unreferenceFromLoop() {
		loop.removeWrapper(this);
	}

	function finalize() {
		Stdlib.free(Pointer.fromRaw(uv));
	}

	static function finalizer(wrapper:Wrapper) {
		cnt--;
		untyped __cpp__('printf("[W]rappers destroyed: %d\\n", {0})', cnt);

		wrapper.finalize();
	}
}