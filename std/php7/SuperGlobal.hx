package php;



class SuperGlobal {
    /**
        @see http://php.net/manual/en/reserved.variables.globals.php
    **/
    public static var GLOBALS (get,never):NativeAssocArray<Dynamic>;
    static inline function get_GLOBALS() return untyped __php__("$GLOBALS");

    /**
        @see http://php.net/manual/en/reserved.variables.server.php
    **/
    public static var _SERVER (get,never):NativeAssocArray<Dynamic>;
    static inline function get__SERVER() return untyped __php__("$_SERVER");

    /**
        @see http://php.net/manual/en/reserved.variables.get.php
    **/
    public static var _GET (get,never):NativeAssocArray<Dynamic>;
    static inline function get__GET() return untyped __php__("$_GET");

    /**
        @see http://php.net/manual/en/reserved.variables.post.php
    **/
    public static var _POST (get,never):NativeAssocArray<Dynamic>;
    static inline function get__POST() return untyped __php__("$_POST");

    /**
        @see http://php.net/manual/en/reserved.variables.files.php
    **/
    public static var _FILES (get,never):NativeAssocArray<Dynamic>;
    static inline function get__FILES() return untyped __php__("$_FILES");

    /**
        @see http://php.net/manual/en/reserved.variables.cookie.php
    **/
    public static var _COOKIE (get,never):NativeAssocArray<Dynamic>;
    static inline function get__COOKIE() return untyped __php__("$_COOKIE");

    /**
        @see http://php.net/manual/en/reserved.variables.session.php
    **/
    public static var _SESSION (get,never):NativeAssocArray<Dynamic>;
    static inline function get__SESSION() return untyped __php__("$_SESSION");

    /**
        @see http://php.net/manual/en/reserved.variables.request.php
    **/
    public static var _REQUEST (get,never):NativeAssocArray<Dynamic>;
    static inline function get__REQUEST() return untyped __php__("$_REQUEST");

    /**
        @see http://php.net/manual/en/reserved.variables.env.php
    **/
    public static var _ENV (get,never):NativeAssocArray<Dynamic>;
    static inline function get__ENV() return untyped __php__("$_ENV");

}