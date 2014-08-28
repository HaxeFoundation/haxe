package unit.issues;

class Issue1997 extends Test {
    function test() {
        var arr = [];
        [1,2,3].map(arr.push);
        eq(arr.length, 3);
        eq(arr[0], 1);
        eq(arr[1], 2);
        eq(arr[2], 3);
    }
}
