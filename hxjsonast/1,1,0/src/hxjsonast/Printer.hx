package hxjsonast;

/**
    Printer for the `Json` values.
    Outputs a valid JSON string. Positions are ignored.
**/
class Printer {
    /**
        Output a JSON string for given `json` value.

        If `space` is specified, the output will be pretty-printed using given
        `space` value for indentation.
    **/
    public static function print(json:Json, ?space:String):String {
        var printer = new Printer(space);
        printer.write(json);
        return printer.buf.toString();
    }

    var buf : #if flash flash.utils.ByteArray #else StringBuf #end;
    var indent:String;
    var pretty:Bool;
    var nind:Int;

    function new(space:String) {
        this.indent = space;
        this.pretty = space != null;
        this.nind = 0;

        #if flash
        buf = new flash.utils.ByteArray();
        buf.endian = flash.utils.Endian.BIG_ENDIAN;
        buf.position = 0;
        #else
        buf = new StringBuf();
        #end
    }

    inline function ipad():Void {
        if (pretty) add(StringTools.lpad('', indent, nind * indent.length));
    }

    inline function newl():Void {
        if (pretty) addChar('\n'.code);
    }

    function write(json:Json) {
        switch (json.value) {
            case JObject(fields):
                addChar('{'.code);
                var len = fields.length;
                var last = len - 1;
                var first = true;
                for (i in 0...len) {
                    var f = fields[i];
                    if (first) {
                        nind++;
                        first = false;
                    } else {
                        addChar(','.code);
                    }
                    newl();
                    ipad();
                    quote(f.name);
                    addChar(':'.code);
                    if (pretty) addChar(' '.code);
                    write(f.value);
                    if (i == last) {
                        nind--;
                        newl();
                        ipad();
                    }
                }
                addChar('}'.code);
            case JNumber(s):
                add(s);
            case JString(s):
                quote(s);
            case JArray(values):
                addChar('['.code);
                var len = values.length;
                var last = len - 1;
                for (i in 0...len) {
                    if (i > 0) addChar(','.code) else nind++;
                    newl();
                    ipad();
                    write(values[i]);
                    if (i == last) {
                        nind--;
                        newl();
                        ipad();
                    }
                }
                addChar(']'.code);
            case JBool(b):
                add(if (b) 'true' else 'false');
            case JNull:
                add('null');
        }
    }

    @:extern inline function addChar(c:Int) {
        #if flash
        buf.writeByte(c);
        #else
        buf.addChar(c);
        #end
    }

    @:extern inline function add(v:String) {
        #if flash
        // argument is not always a string but will be automatically casted
        buf.writeUTFBytes(v);
        #else
        buf.add(v);
        #end
    }

    function quote(s:String) {
        #if (neko || php || cpp)
        if (s.length != haxe.Utf8.length(s)) {
            quoteUtf8(s);
            return;
        }
        #end
        addChar('"'.code);
        var i = 0;
        while (true) {
            var c = StringTools.fastCodeAt(s, i++);
            if (StringTools.isEof(c))
                break;
            switch (c) {
                case '"'.code: add('\\"');
                case '\\'.code: add('\\\\');
                case '\n'.code: add('\\n');
                case '\r'.code: add('\\r');
                case '\t'.code: add('\\t');
                case 8: add('\\b');
                case 12: add('\\f');
                default:
                    #if flash
                    if (c >= 128) add(String.fromCharCode(c)) else addChar(c);
                    #else
                    addChar(c);
                    #end
            }
        }
        addChar('"'.code);
    }

    #if (neko || php || cpp)
    function quoteUtf8(s:String) {
        var u = new haxe.Utf8();
        haxe.Utf8.iter(s, function(c) {
            switch (c) {
                case '\\'.code, '"'.code: u.addChar('\\'.code); u.addChar(c);
                case '\n'.code: u.addChar('\\'.code); u.addChar('n'.code);
                case '\r'.code: u.addChar('\\'.code); u.addChar('r'.code);
                case '\t'.code: u.addChar('\\'.code); u.addChar('t'.code);
                case 8: u.addChar('\\'.code); u.addChar('b'.code);
                case 12: u.addChar('\\'.code); u.addChar('f'.code);
                default: u.addChar(c);
            }
        });
        buf.add('"');
        buf.add(u.toString());
        buf.add('"');
    }
    #end
}
