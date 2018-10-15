package unit.issues;

private enum abstract TP_KIND<T>(Int) {

    public function new () this = 0;

    var TP_REF = 0;
    var TP_V08 = 1;
    var TP_V16 = 2;
    var TP_V32 = 3;
    var TP_V64 = 4;
    var TP_UNKNOWN = 5;
    var ___ = -1;
}

class Issue5179 extends unit.Test {
	function test() {

	}
}