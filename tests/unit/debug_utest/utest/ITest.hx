package utest;

#if !macro
@:autoBuild(utest.Macro.buildTests())
#end
interface ITest {
#if !macro
	public function runTests() : Void;
#end
}