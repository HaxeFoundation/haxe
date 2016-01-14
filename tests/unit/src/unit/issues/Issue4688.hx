package unit.issues;


class Issue4688 extends Test {
    #if php
    function test() {
        var msg = 'Should be converted to exception if "phpNoErrorHandling" is not defined';
        var errorLevel = untyped __php__('E_USER_ERROR');

        try {
            untyped __call__('trigger_error', msg, errorLevel);
            #if !phpNoErrorHandling
            eq('fail', 'Should not be reached if "phpNoErrorHandling" is not defined');
            #end
        } catch (error:String) {
            #if phpNoErrorHandling
            eq('fail', 'Should not be reached if "phpNoErrorHandling" is defined');
            #end
        }
    }
    #end
}
