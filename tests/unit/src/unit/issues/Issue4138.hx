package unit.issues;

#if worker
import js.html.compat.ArrayBuffer;

@:keep class Issue4138 {
	static function main():Void {
		var b = new js.html.ArrayBuffer(1);
		// `Type.getClassName` uses `js.Boot.__resolveNativeClass`, thus will test for #4183 too.
		(untyped postMessage)(Type.getClassName(Type.getClass(b)));
	}
}
#else
class Issue4138 extends unit.Test
{
	#if js
	public function test()
	{
		var NativeArrayBuffer = untyped Function("return typeof ArrayBuffer != 'undefined' ? ArrayBuffer : null")();
		if (js.Browser.supported && untyped js.Browser.window.Worker && NativeArrayBuffer != null){
			async(function(_, cb){
				var myWorker = new js.html.Worker("bin/Issue4138_Worker.js");
				myWorker.onmessage = function(e:Dynamic){
					var className:String = e.data;
					f(className == Type.getClassName(js.html.compat.ArrayBuffer));
					cb(true);
				}
				myWorker.onerror = function(e){
					log('${e.message} (${e.filename}:${e.lineno})');
					t(false);
				}
			}, null, true);
		}
	}
	#end
}
#end