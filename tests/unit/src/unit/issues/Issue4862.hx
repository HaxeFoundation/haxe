package unit.issues;

#if js
@:native("__issue4862__http_status")
private extern enum abstract HttpStatus(Int) to Int {
    var Ok;
    var NotFound;

    static function __init__():Void {
        js.Syntax.code("var __issue4862__http_status = {Ok: 200, NotFound: 404};");
    }
}
#end

class Issue4862 extends Test {
    #if js
    function test() {
        var a = Ok;
        eq(200, a);
        var b = HttpStatus.NotFound;
        eq(404, b);
        t(switch (a) { case HttpStatus.Ok: true; default: false; });
        t(switch (b) { case HttpStatus.NotFound: true; default: false; });
        t(switch (a) { case Ok: true; default: false; });
        t(switch (b) { case NotFound: true; default: false; });
    }
    #end
}
