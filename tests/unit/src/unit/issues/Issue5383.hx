package unit.issues;

class Issue5383 extends unit.Test {
#if php
    static var issue = Issue5383;

    function test() {
        //Should generate instance access
        issue.doStuff();
        issue.issue.doStuff();
        t(true);
    }

    static var anchor:Float;
    static function doStuff() {
        anchor = Math.random();
    }
#end
}