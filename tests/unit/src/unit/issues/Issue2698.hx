package unit.issues;
import unit.Test;

private abstract IntString(String) {
    @:from static function fromInt(i:Int):IntString return cast Std.string(i);
    @:to function toInt():Int return Std.parseInt(this);
}

class Issue2698 extends Test {
	function test() {
		var a:IntString = 1;
		var b:Int = 1;
		var c:IntString = 2;
		var d:Int = 2;
		
		t(a == a);
		t(a == b);
		f(a == c);
		f(a == d);
		
		t(b == a);
		t(b == b);
		f(b == c);
		f(b == d);
		
		f(c == a);
		f(c == b);
		t(c == c);
		t(c == d);
		
		f(d == a);
		f(d == b);
		t(d == c);
		t(d == d);
	}
}