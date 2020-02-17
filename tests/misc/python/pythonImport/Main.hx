import utest.Assert;

@:pythonImport("native_python.sample", "A")
extern class ExternClass {
    function new();
    function f(v:Int):Int;
}

@:pythonImport("native_python.sample", "A.Nested")
extern class ExternNestedClass {
    function new();
    function f(v:Int):Int;
}

@:pythonImport("native_python.sample")
extern class ExternModule {
    static function f(v:Int):Int;
}

@:pythonImport("inexistant", "AZAZA", ignoreError=true)
extern class InexistantExtern1 {}

@:pythonImport("inexistant", "AZAZA.ZAZA", ignoreError=true)
extern class InexistantExtern2 {}

@:pythonImport("inexistant", ignoreError=true)
extern class InexistantExtern3 {}

class Main extends utest.Test {

    function testExtern() {
        Assert.equals(new ExternClass().f(1), 2);
        Assert.equals(new ExternNestedClass().f(1), 3);
        Assert.equals(ExternModule.f(1), 4);
    }

    static function main() {
        var runner = new utest.Runner();
        runner.addCase(new Main());
		utest.ui.Report.create(runner);
		runner.run();
    }
}
