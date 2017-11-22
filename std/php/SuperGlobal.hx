package php;

class SuperGlobal {
    /**
        @see http://php.net/manual/en/reserved.variables.globals.php
    **/
    public static var GLOBALS (get,never):NativeAssocArray<Dynamic>;
    static inline function get_GLOBALS() return Syntax.code("$GLOBALS");

    /**
        @see http://php.net/manual/en/reserved.variables.server.php
    **/
    public static var _SERVER (get,never):NativeAssocArray<Dynamic>;
    static inline function get__SERVER() return Syntax.code("$_SERVER");

    /**
        @see http://php.net/manual/en/reserved.variables.get.php
    **/
    public static var _GET (get,never):NativeAssocArray<Dynamic>;
    static inline function get__GET() return Syntax.code("$_GET");

    /**
        @see http://php.net/manual/en/reserved.variables.post.php
    **/
    public static var _POST (get,never):NativeAssocArray<Dynamic>;
    static inline function get__POST() return Syntax.code("$_POST");

    /**
        @see http://php.net/manual/en/reserved.variables.files.php
    **/
    public static var _FILES (get,never):NativeAssocArray<Dynamic>;
    static inline function get__FILES() return Syntax.code("$_FILES");

    /**
        @see http://php.net/manual/en/reserved.variables.cookie.php
    **/
    public static var _COOKIE (get,never):NativeAssocArray<Dynamic>;
    static inline function get__COOKIE() return Syntax.code("$_COOKIE");

    /**
        @see http://php.net/manual/en/reserved.variables.session.php
    **/
    public static var _SESSION (get,never):NativeAssocArray<Dynamic>;
    static inline function get__SESSION() return Syntax.code("$_SESSION");

    /**
        @see http://php.net/manual/en/reserved.variables.request.php
    **/
    public static var _REQUEST (get,never):NativeAssocArray<Dynamic>;
    static inline function get__REQUEST() return Syntax.code("$_REQUEST");

    /**
        @see http://php.net/manual/en/reserved.variables.env.php
    **/
    public static var _ENV (get,never):NativeAssocArray<Dynamic>;
    static inline function get__ENV() return Syntax.code("$_ENV");

}