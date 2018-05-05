package php;

import haxe.extern.Rest;

/**
    @see http://php.net/manual/en/class.closure.php
**/
@:native('Closure')
extern class Closure {
    public function bindTo( newthis:{}, newscope:Dynamic = "static" ) : Closure;
    public function call ( newthis:{}, args:Rest<Dynamic> ) : Dynamic;
}