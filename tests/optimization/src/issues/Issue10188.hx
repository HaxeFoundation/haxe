package issues;

enum abstract Value(Int) to Int {
	final V1 = 1;
	final V2 = 2;

	public inline function toString():String {
		return switch (cast this:Value) {
			case V1: 'V1';
			case V2: 'V2';
		}
	}
}

enum abstract ActorStateType(Int) from Int to Int {
    var GoingLeft = 0;
    var GoingRight = 1;
}

class Issue10188 {
	@:js('
		var _g = [];
		if(issues_Issue10188.has(1)) {
			_g.push("V1");
		}
		issues_Issue10188.use(_g);
	')
	static function testKevin() {
		use([for(v in [V1]) if(has(v)) v.toString()]);
	}


	static function has(v:Value) {
		return true;
	}

	@:js('
		var state = 0;
		if(Math.random() > 0.5) {
			state = 0;
			issues_Issue10188.use("left");
		}
		issues_Issue10188.use(state);
	')
	static function testNotKevin() {
			var state = GoingLeft;
			inline function set(s) {
				state = s;
				use(getAnim(s));
			}
			if(Math.random() > 0.5) set(GoingLeft);
			use(state);
	}

    static inline function getAnim(state:ActorStateType):String {
        return switch(state) {
            case GoingLeft: 'left';
            case GoingRight: 'right';
        }
    }

	@:pure(false)
	static public function use<T>(t:T) { return t; }
}