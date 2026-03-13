package hxjsonast;

import hxjsonast.Json;

/**
    A parser for the JSON data.
    Throws exceptions on invalid JSON.
**/
class Parser {
    /**
        Parse given `source` text into a `Json` value using `filename` for
        positions.

        If `source` contains invalid JSON an exception will be thrown.
    **/
    public static inline function parse(source:String, filename:String):Json {
        return new Parser(source, filename).doParse();
    }

    var source:String;
    var filename:String;
    var pos:Int;

    function new(source:String, filename:String) {
        this.source = source;
        this.filename = filename;
        this.pos = 0;
    }

    function doParse():Json {
        var result = parseRec();
        var c;
        while (!StringTools.isEof(c = nextChar())) {
            switch(c) {
                case ' '.code, '\r'.code, '\n'.code, '\t'.code:
                    // allow trailing whitespace
                default:
                    invalidChar();
            }
        }
        return result;
    }

    function parseRec():Json {
        while (true) {
            var c = nextChar();
            switch(c) {
                case ' '.code | '\r'.code | '\n'.code | '\t'.code:
                    // loop
                case '{'.code:
                    var fields = new Array<JObjectField>();
                    var names = new haxe.ds.StringMap();
                    var field = null;
                    var fieldPos = null;
                    var comma:Null<Bool> = null;
                    var startPos = pos - 1;
                    while (true) {
                        switch (nextChar()) {
                            case ' '.code | '\r'.code | '\n'.code | '\t'.code:
                                // loop
                            case '}'.code:
                                if (field != null || comma == false)
                                    invalidChar();
                                return mk(mkPos(startPos, pos), JObject(fields));
                            case ':'.code:
                                if (field == null)
                                    invalidChar();
                                fields.push(new JObjectField(field, fieldPos, parseRec()));
                                field = null;
                                fieldPos = null;
                                comma = true;
                            case ','.code:
                                if (comma)
                                    comma = false;
                                else
                                    invalidChar();
                            case '"'.code:
                                if (field != null || comma)
                                    invalidChar();
                                var fieldStartPos = pos - 1;
                                field = parseString();
                                fieldPos = mkPos(fieldStartPos, pos);
                                if (names.exists(field))
                                    throw new Error('Duplicate field name "$field"', fieldPos);
                                else
                                    names.set(field, true);
                            default:
                                invalidChar();
                        }
                    }
                case '['.code:
                    var values = [];
                    var comma:Null<Bool> = null;
                    var startPos = pos - 1;
                    while (true) {
                        switch (nextChar()) {
                            case ' '.code | '\r'.code | '\n'.code | '\t'.code:
                                // loop
                            case ']'.code:
                                if (comma == false)
                                    invalidChar();
                                return mk(mkPos(startPos, pos), JArray(values));
                            case ','.code:
                                if (comma)
                                    comma = false;
                                else
                                    invalidChar();
                            default:
                                if (comma)
                                    invalidChar();
                                pos--;
                                values.push(parseRec());
                                comma = true;
                        }
                    }
                case 't'.code:
                    var save = pos;
                    if (nextChar() != 'r'.code || nextChar() != 'u'.code || nextChar() != 'e'.code) {
                        pos = save;
                        invalidChar();
                    }
                    return mk(mkPos(save - 1, pos), JBool(true));
                case 'f'.code:
                    var save = pos;
                    if (nextChar() != 'a'.code || nextChar() != 'l'.code || nextChar() != 's'.code || nextChar() != 'e'.code) {
                        pos = save;
                        invalidChar();
                    }
                    return mk(mkPos(save - 1, pos), JBool(false));
                case 'n'.code:
                    var save = pos;
                    if (nextChar() != 'u'.code || nextChar() != 'l'.code || nextChar() != 'l'.code) {
                        pos = save;
                        invalidChar();
                    }
                    return mk(mkPos(save - 1, pos), JNull);
                case '"'.code:
                    var save = pos;
                    var s = parseString();
                    return mk(mkPos(save - 1, pos), JString(s));
                case '0'.code, '1'.code,'2'.code,'3'.code,'4'.code,'5'.code,'6'.code,'7'.code,'8'.code,'9'.code,'-'.code:
                    return parseNumber(c);
                default:
                    invalidChar();
            }
        }
    }

    function parseString():String {
        var start = pos;
        var buf = null;
        while (true) {
            var c = nextChar();
            if (c == '"'.code)
                break;
            if (c == '\\'.code) {
                if (buf == null)
                    buf = new StringBuf();
                buf.addSub(source, start, pos - start - 1);
                c = nextChar();
                switch(c) {
                    case "r".code:
                        buf.addChar("\r".code);
                    case "n".code:
                        buf.addChar("\n".code);
                    case "t".code:
                        buf.addChar("\t".code);
                    case "b".code:
                        buf.addChar(8);
                    case "f".code:
                        buf.addChar(12);
                    case "/".code | '\\'.code | '"'.code:
                        buf.addChar(c);
                    case 'u'.code:
                        var uc = Std.parseInt("0x" + source.substr(pos, 4));
                        pos += 4;
                        #if (neko || (!haxe4 && (php || cpp || lua) || (cpp && !hxcpp_smart_strings)))
                        if (uc <= 0x7F)
                            buf.addChar(uc);
                        else if (uc <= 0x7FF) {
                            buf.addChar(0xC0 | (uc >> 6));
                            buf.addChar(0x80 | (uc & 63));
                        } else if (uc <= 0xFFFF) {
                            buf.addChar(0xE0 | (uc >> 12));
                            buf.addChar(0x80 | ((uc >> 6) & 63));
                            buf.addChar(0x80 | (uc & 63));
                        } else {
                            buf.addChar(0xF0 | (uc >> 18));
                            buf.addChar(0x80 | ((uc >> 12) & 63));
                            buf.addChar(0x80 | ((uc >> 6) & 63));
                            buf.addChar(0x80 | (uc & 63));
                        }
                        #else
                        buf.addChar(uc);
                        #end
                    default:
                        throw new Error("Invalid escape sequence \\" + String.fromCharCode(c), mkPos(pos - 2, pos));
                }
                start = pos;
            }
            #if (neko || (!haxe4 && (php || cpp)))
            // ensure utf8 chars are not cut
            else if (c >= 0x80) {
                pos++;
                if (c >= 0xFC) pos += 4;
                else if (c >= 0xF8) pos += 3;
                else if (c >= 0xF0) pos += 2;
                else if (c >= 0xE0) pos++;
            }
            #end
            else if (StringTools.isEof(c)) {
                pos--;
                throw new Error("Unclosed string", mkPos(start - 1, pos));
            }
        }
        if (buf == null) {
            return source.substr(start, pos - start - 1);
        } else {
            buf.addSub(source,start, pos - start - 1);
            return buf.toString();
        }
    }

    inline function parseNumber(c:Int):Json {
        var start = pos - 1;
        var minus = c == '-'.code;
        var digit = !minus;
        var zero = c == '0'.code;
        var point = false;
        var e = false;
        var pm = false;
        var end = false;
        while (true) {
            switch (nextChar()) {
                case '0'.code:
                    if (zero && !point)
                        invalidNumber(start);
                    if (minus) {
                        minus = false;
                        zero = true;
                    }
                    digit = true;
                case '1'.code | '2'.code | '3'.code | '4'.code | '5'.code | '6'.code | '7'.code | '8'.code | '9'.code:
                    if (zero && !point)
                        invalidNumber(start);
                    if (minus)
                        minus = false;
                    digit = true;
                    zero = false;
                case '.'.code:
                    if (minus || point || e)
                        invalidNumber(start);
                    digit = false;
                    point = true;
                case 'e'.code | 'E'.code:
                    if (minus || zero || e)
                        invalidNumber(start);
                    digit = false;
                    e = true;
                case '+'.code | '-'.code:
                    if (!e || pm)
                        invalidNumber(start);
                    digit = false; pm = true;
                default:
                    if (!digit)
                        invalidNumber(start);
                    pos--;
                    end = true;
            }
            if (end)
                break;
        }
        var s = source.substr(start, pos - start);
        return mk(mkPos(start, pos), JNumber(s));
    }

    inline function nextChar():Int {
        return StringTools.fastCodeAt(source, pos++);
    }

    inline function mk(pos:Position, value:JsonValue):Json {
        return new Json(value, pos);
    }

    inline function mkPos(min:Int, max:Int):Position {
        return new Position(filename, min, max);
    }

    function invalidChar() {
        pos--; // rewind
        throw new Error("Invalid character: " + source.charAt(pos), mkPos(pos, pos + 1));
    }

    function invalidNumber(start:Int) {
        throw new Error("Invalid number: " + source.substring(start, pos), mkPos(start, pos));
    }
}
