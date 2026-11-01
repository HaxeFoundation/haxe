package unit.issues;

#if js
import haxe.extern.EitherType;
import js.html.Blob;
import js.lib.ArrayBuffer;
import js.lib.ArrayBufferView;

private typedef BlobPart = EitherType<EitherType<ArrayBufferView, ArrayBuffer>, EitherType<Blob, String>>;
#end

class Issue7596 extends unit.Test {
	#if js
	function test() {
		var arrayBuffers = [new ArrayBuffer(1)];
		var parts:Array<BlobPart> = arrayBuffers;
		eq(1, parts.length);
		// Only has to type: Blob is not available in every JS environment the tests run in
		function makeBlob() {
			return new Blob(arrayBuffers);
		}
	}
	#end
}
