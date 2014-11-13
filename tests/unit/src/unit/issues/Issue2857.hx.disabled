package unit.issues;

class Issue2857 extends unit.Test {
#if js
	function testElement() {
		if (js.Browser.supported) {
			var vid = js.Browser.document.createVideoElement();
			t(Std.is(vid, js.html.VideoElement));
			t(Std.is(vid, js.html.Element));
			f(Std.is(vid, haxe.Http));
			f(Std.is(vid, js.html.ArrayBuffer));
			eq(Type.getClass(vid), js.html.VideoElement);
		}
	}
#end
}
