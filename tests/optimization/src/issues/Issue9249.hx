package issues;

class Issue9249 {
	@:js('
		return _pulsingEffect != null && (_image != null || !issues_Issue9249._hasImage()) && (!_doAnimate || _animation != null && _animation.animationPlaying);
	')
	static function test(_pulsingEffect:Any, _image:Any, _doAnimate:Bool, _animation:Dynamic) {
		return _pulsingEffect != null
			&& (_image != null || !_hasImage())
			&& (!_doAnimate || (_animation != null && _animation.animationPlaying));
	}
	static function _hasImage() return true;

	@:js('
		return Std.random(0) == 0 && a && b;
	')
	static function and(a:Bool, b:Bool):Bool {
		var a = Std.random(0) == 0 && a && b;
		return a;
	}
	@:js('
		return a && b && time == 0;
	')
	static function and2(a:Bool, b:Bool, time:Int):Bool {
		return a && b && time == 0;
	}
}
