package issues;

private extern class Element {
	static function get():Element;
	var offset(default,null) : Int;
	var style(default,null) : CSSStyleDeclaration;
}

@:pure
private extern class PureElement {
	static function get():PureElement;
	var offset(default,null) : Int;
	var style(default,null) : CSSStyleDeclaration;
}
private extern class CSSStyleDeclaration {
	var verticalAlign : Int;
}

class Issue9943 {
	static final foo = Std.random(0);

	@:js('
		var el = issues._Issue9943.Element.get();
		var a = el.offset + issues_Issue9943.foo;
		el.style.verticalAlign = 1;
		var b = el.offset + issues_Issue9943.foo;
		el.style.verticalAlign = 2;
		var c = el.offset + issues_Issue9943.foo;
		issues_Issue9943.values(a,b,c);
	')
	static function test() {
		final el = Element.get();
		final a = (el.offset + foo);
		el.style.verticalAlign = 1;
		final b = (el.offset + foo);
		el.style.verticalAlign = 2;
		final c = (el.offset + foo);
		values(a, b, c);
	}
	@:js('
		var el = issues._Issue9943.PureElement.get();
		el.style.verticalAlign = 1;
		el.style.verticalAlign = 2;
		issues_Issue9943.values(el.offset + issues_Issue9943.foo,el.offset + issues_Issue9943.foo,el.offset + issues_Issue9943.foo);
	')
	static function test2() {
		final el = PureElement.get();
		final a = (el.offset + foo);
		el.style.verticalAlign = 1;
		final b = (el.offset + foo);
		el.style.verticalAlign = 2;
		final c = (el.offset + foo);
		values(a, b, c);
	}

	static dynamic function values(a, b, c) trace(a, b, c);
}
