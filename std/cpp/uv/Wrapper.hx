package cpp.uv;

/**
	Base class for Haxe objects wrapping libuv entities.
**/
@:allow(cpp.uv)
abstract class Wrapper {
	var uv:RawPointer<cpp.Void>;
	final loop:Loop;

	//these are needed to keep a linked list of active wrappers per loop (see Loop.activeWrappers)
	var prevRef:Null<Wrapper>;
	var nextRef:Null<Wrapper>;

	function new(loop:Loop) {
		this.loop = loop;
		setupUvData();
		cpp.vm.Gc.setFinalizer(this, Function.fromStaticFunction(finalizer));
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
		wrapper.finalize();
	}
}