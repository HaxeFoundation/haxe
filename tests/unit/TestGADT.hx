package unit;

enum Constant<T> {
	CString(s:String):Constant<String>;
	CInt(s:String):Constant<Int>;
	CFloat(s:String):Constant<Float>;
}

enum Binop<S,T> {
	OpAdd:Binop<Float,Float>;
	OpEq:Binop<S,Bool>;
}

enum Expr<T> {
	EConst(c:Constant<T>):Expr<T>;
	EBinop<C>(op:Binop<C,T>, e1:Expr<C>, e2:Expr<C>):Expr<T>;
}

class TestGADT extends Test {

	function testBasic() {
		var ti = 1.22;
		var tb = false;
		
		var e1 = EConst(CFloat("12"));
		var e2 = EConst(CFloat("8"));
		var e3 = EConst(CFloat("12"));
		
		var eadd = EBinop(OpAdd,e1,e2);
		var s = eval(eadd);
		TestType.typedAs(s, ti);
		eq(s,20);
		
		var eeq = EBinop(OpEq,e1,e2);
		var s = eval(eeq);
		TestType.typedAs(s, tb);
		eq(s,false);
		
		var eeq = EBinop(OpEq,e1,e3);
		var s = eval(eeq);
		TestType.typedAs(s, tb);
		eq(s,true);
	}
	
	static function evalConst<T>(c:Constant<T>):T {
		return switch (c) {
			case CString(s): s;
			case CInt(i): Std.parseInt(i);
			case CFloat(f): Std.parseFloat(f);
		}
	}
	
	static function evalBinop<T,C>(op:Binop<C,T>, e1:Expr<C>, e2:Expr<C>):T {
		return switch(op) {
			case OpAdd: eval(e1) + eval(e2);
			case OpEq: eval(e1) == eval(e2);
		}
	}
	
	static function eval<T>(e:Expr<T>):T {
		return switch(e) {
			case EConst(c): evalConst(c);
			case EBinop(_op,_e1,_e2): evalBinop(_op,_e1,_e2); // TODO: this generates some unused variable warnings in macro context (issue #1675?)
		}
	}
}