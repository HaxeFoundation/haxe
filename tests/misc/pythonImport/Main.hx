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

class Main extends haxe.unit.TestCase {

    function testExtern() {
        assertEquals(new ExternClass().f(1), 2);
        assertEquals(new ExternNestedClass().f(1), 3);
        assertEquals(ExternModule.f(1), 4);
    }

    static function main() {
        var runner = new haxe.unit.TestRunner();
        runner.add(new Main());
        var code = runner.run() ? 0 : 1;
        Sys.exit(code);
    }
}
