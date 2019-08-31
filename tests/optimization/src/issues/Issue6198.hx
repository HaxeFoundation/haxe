package issues;

private enum E {
    A;
}
class Issue6198 {
	@:js('
		if(issues_Issue6198.getNull()._hx_index == 0) {
			issues_Issue6198.use("e = E.A");
		}
	')
	static function test1() {
        var e:Null<E> = getNull();
        switch(e)
        {
            case E.A:
                use("e = E.A");
        }
	}

	static function getNull() { return null; }

	@:pure(false) static function use(a) { }
}