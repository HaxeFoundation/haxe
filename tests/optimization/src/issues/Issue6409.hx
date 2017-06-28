package issues;

enum E {
    Flags( v : String );
}

class Issue6409 {
	@:js('
		var issues_E1 = null;
		var f = issues_E.Flags("");
		console.log(f);
	')
	@:analyzer(ignore)
    static function test() {
        var issues_E = null;
        var f : E = Flags("");
        trace(f);
    }
}