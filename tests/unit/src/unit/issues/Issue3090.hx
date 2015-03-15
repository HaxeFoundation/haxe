package unit.issues;
import haxe.xml.Printer;
@:generic
private class A<T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15>{
    public function new(){}
}

class Issue3090 extends Test {
	function test() {
		var a = new A<Printer,Printer,Printer,Printer,Printer,Printer,Printer,Printer,Printer,Printer,Printer,Printer,Printer,Printer,Printer>();
	}
}