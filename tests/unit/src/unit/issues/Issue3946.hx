package unit.issues;

class Issue3946 extends Test {
    #if cs
    function test() {
        var arr = cs.Lib.arrayAlloc(10);
        eq(arr.length, 10);
				arr[0] = 5;
				eq(arr[0],5);
    }
    #end
}
