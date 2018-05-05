package issues;

class Issue3878 {
	@:js('
		var v = "hello";
	')
	@:analyzer(no_const_propagation, no_local_dce)
	static function test() {
        var v = (function(id) return id : String->String)("hello");
	}
}