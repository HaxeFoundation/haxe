package unit.issues;

enum E1 {
	A(i:Int);
	B;
}

enum E2 {
	A(s:String);
	B;
}

#if !macro
@:build(unit.UnitBuilder.build("issues", ".issue.hx"))
#end
class TestIssues extends Test {

}
