package issues;

class Issue10908 {
	@:js('
		var el = null;
		el.style.width = "" + el.videoWidth + "px";
		el.style.height = "" + el.videoHeight + "px";
	')
	static function test() {
		final el:js.html.VideoElement = null;
		el.style.width = '${el.videoWidth}px';
		el.style.height = '${el.videoHeight}px';
	}
}
