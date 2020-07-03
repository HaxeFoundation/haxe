package issues;

class Issue9617 {
	@:js('
		try {
			issues_Issue9617.dummy();
		} catch( _g ) {
			var _g1 = haxe_Exception.caught(_g);
			var e = _g1;
			var _g2 = issues_Issue9617.process(_g1);
			if(_g2 != null) {
				var e = _g2;
				issues_Issue9617.dummy();
			}
		}
	')
	@:analyzer(no_local_dce)
	static public function test() {
		try {
			dummy();
		} catch(e) {
			switch process(e) {
				case null:
				case e: dummy();
			}
		}
	}

	@:pure(false) static function process(e) {
		return e;
	}

	@:pure(false) static function dummy() {}
}