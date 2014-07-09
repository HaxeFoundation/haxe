package unit.issues;
import unit.Test;

class Issue3071 extends Test {


    function test() {
    	// join
    	var a : { function join (s:String):String; } = [1,2];
    	eq("1,2", a.join(","));

    	var a : Dynamic = [1,2];
    	eq("1,2", a.join(","));

    	// pop
    	var a : { function pop ():Int; } = [1,2];
    	a.pop();
    	var a : Array<Int> = cast a;
    	eq(1, a.length);
    	eq(a[0], 1);

    	var a : Dynamic = [1,2];
    	a.pop();
    	var a : Array<Int> = cast a;
    	eq(1, a.length);
    	eq(a[0], 1);

    	// push
    	var a : { function push (x:Int):Int; } = [];
    	a.push(1);
    	var a : Array<Int> = cast a;
    	eq(1, a.length);
    	eq(a[0], 1);

    	var a : Dynamic = [];
    	a.push(1);
    	var a : Array<Int> = cast a;
    	eq(1, a.length);
    	eq(a[0], 1);

    	// shift
    	var a : { function shift ():Int; } = [1,2];
    	a.shift();
    	var a : Array<Int> = cast a;
    	eq(1, a.length);
    	eq(a[0], 2);

    	var a : Dynamic = [1,2];
    	a.shift();
    	var a : Array<Int> = cast a;
    	eq(1, a.length);
    	eq(a[0], 2);

    }

}