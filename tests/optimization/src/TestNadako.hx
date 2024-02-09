import TestJs.use;

class TestNadako {
	@:js('var r = [];
	var _g_current = 0;
	while(_g_current < _requirements.length) {
		var _g_value = _requirements[_g_current];
		TestJs.use(_g_value);
		r[_g_current++] = _g_value;
	}')
	static function testNadako1(_requirements:Array<{}>) {
		var r = [];
		for (i => resourceData in _requirements) {
			use(resourceData);
			r[i] = resourceData;
		}
	}
}