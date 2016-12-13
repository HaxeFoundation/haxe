package unit.issues;

class Issue5383 extends unit.Test {
#if php
	static var sys = Sys;
    static var issue = Issue5383;

    function test() {
        //Should generate instance access
        sys.print('TEST');
        issue.sys.print('TEST');
        t(true);
    }
#end
}