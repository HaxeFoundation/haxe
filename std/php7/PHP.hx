package php7;

/**
    Special extern class to support PHP language specifics.
    Don't use these functions unless you are really sure what you are doing.
**/
extern class PHP {
    /**
        `==` operator in Haxe uses strict equality check.
        This method allows to compare values with non-strict equality check.
    **/
    static function equal( value1:Dynamic, value2:Dynamic ) : Bool;

    /**
        Generates `(int)$value`
    **/
    static function int( value:Dynamic ) : Int;

    /**
        Generates `(float)$value`
    **/
    static function float( value:Dynamic ) : Float;

    /**
        Generates `(string)$value`
    **/
    static function string( value:Dynamic ) : String;

    /**
        Generates `(bool)$value`
    **/
    static function bool( value:Dynamic ) : Bool;

    /**
        Generates `(object)$value`
    **/
    static function object( value:Dynamic ) : Dynamic;

    /**
        Generates `(array)$value`
    **/
    static function array( value:Dynamic ) : NativeArray;

    /**
        Ggenerates `$value instanceof $phpClassName`.
        `type` only accepts direct class names. That means `Type.resolveClass('MyClass')` is not allowed, but `MyClass` is.
    **/
    static function instanceof( value:Dynamic,  type:Class<Dynamic> ) : Bool;// return _instanceof(value, type);
    //This trick is required to make compiler assign expressions to variables.
    //Otherwise PHP will complain if `type` or `value` are not variables, but some another expressions.
    // static private function _instanceof( value:Dynamic,  type:Class<Dynamic> ) : Bool;

    /**
        ```
        PHP.foreach(collection, function(key, value) trace(key, value));
        ```
        generates:
        ```
        foreach($collection as $key => $value) {
            trace($key, $value);
        }
        ```
    **/
    static function foreach<TKey,TValue>( collection:Dynamic, body:TKey->TValue->Void ) : Void;
}