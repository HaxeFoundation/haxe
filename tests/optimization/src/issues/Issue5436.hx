package issues;

class Issue5436 {
	@:js('
		var f = function() {};
		issues_Issue5436.call(f);
		issues_Issue5436.call(f);
	')
	@:analyzer(no_local_dce)
    static function test() {
        inlineMe(function() { });
    }

    static inline function inlineMe(f:() -> Void) {
        call(f);
        call(f);
    }

	static function call(f:() -> Void) { }
}