package unit.issues;

class Issue6177 extends unit.Test {
    function test() {
        var list = new List();
        list.push('a');
        switch(list.map(A).first()) {
            case A('a'): t(true);
            case _: t(false);
        }
    }
}

private enum TestEnum {
    A(a:String);
}