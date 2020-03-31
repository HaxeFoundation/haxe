package unit;

import haxe.Rest;

class TestRest extends Test {
    function testArrayAccess() {
        function rest(a:Int, b:Int, r:Rest<Int>) {
            return r[2];
        }
        eq(123, rest(1, 2, 0, 0, 123, 0));
    }

    function testLength() {
        function rest(r:Rest<Int>) {
            return r.length;
        }
        eq(4, rest(1, 2, 3, 4));
    }

#if !lua
    function testRestReturn(){
        function rest(r:Rest<Int>){
            return r;
        }
        eq(4, rest(1,2,3,4)[3]);
    }
#end

    function testIterator() {
        function rest(r:Rest<Int>):Array<Int> {
            return [for(i in r) i];
        }
        aeq([3, 2, 1], rest(3, 2, 1));
    }

    function testKeyValueIterator() {
        function rest(r:Rest<Int>):{keys:Array<Int>, values:Array<Int>} {
            var keys = [];
            var values = [];
            for(k => v in r) {
                keys.push(k);
                values.push(v);
            }
            return {keys:keys, values:values}
        }
        var r = rest(3, 2, 1, 0);
        aeq([0, 1, 2, 3], r.keys);
        aeq([3, 2, 1, 0], r.values);
    }
}
