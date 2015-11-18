package unit.issues;

class Issue4644 extends Test {
    function test() {
        #if js
        var isHaxeError;
        untyped __js__(
            "try {{
                {0};
            }} catch (e) {{
                {1} = (e instanceof js__$Boot_HaxeError);
            }}",
            throw (new js.Error() : Dynamic),
            isHaxeError
        );
        f(isHaxeError);
        #end
    }
}
