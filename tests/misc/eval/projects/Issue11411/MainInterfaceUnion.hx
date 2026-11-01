interface A {}
interface B {}
class C implements A implements B {}

class Parent {
	function f<TC:C>(a:TC, b:TC) {}
}

class Child extends Parent {
	override function f<TA:A, TB:B>(a:TA, b:TB) {}
}

function main() {}