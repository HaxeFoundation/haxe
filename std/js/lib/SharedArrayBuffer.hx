package js.lib;

/**
	The SharedArrayBuffer object is used to represent a generic, fixed-length raw binary data buffer, similar to the ArrayBuffer object, but in a way that they can be used to create views on shared memory.
	A SharedArrayBuffer is not a Transferable Object, unlike an ArrayBuffer which is transferable.

	Documentation [SharedArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer/contributors.txt), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("SharedArrayBuffer")
extern class SharedArrayBuffer {
	final byteLength:Int;

	function new(?length:Int):Void;

	function slice(?begin:Int, ?end:Int):ArrayBuffer;
}
