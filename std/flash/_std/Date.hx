/*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

@:coreApi extern class Date
{
    function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void;
    function getTime() : Float;
    function getHours() : Int;
    function getMinutes() : Int;
    function getSeconds() : Int;
    function getFullYear() : Int;
    function getMonth() : Int;
    function getDate() : Int;
    function getDay() : Int;
    
    inline function toString():String
    {
        var m = this.getMonth() + 1;
        var d = this.getDate();
        var h = this.getHours();
        var mi = this.getMinutes();
        var s = this.getSeconds();
        
        return this.getFullYear()
            +"-"+(if( m < 10 ) "0"+m else ""+m)
            +"-"+(if( d < 10 ) "0"+d else ""+d)
            +" "+(if( h < 10 ) "0"+h else ""+h)
            +":"+(if( mi < 10 ) "0"+mi else ""+mi)
            +":"+(if( s < 10 ) "0"+s else ""+s);
    }

    inline static function now() : Date {
        return untyped __new__(Date);
    }

    inline static function fromTime( t : Float ) : Date untyped {
        var date : Date = __new__(Date);
        date.setTime(t);
        return date;
    }
    
    inline static function fromString( s : String ) : Date untyped {
        var result:Date;
        switch( s.length ) {
            case 8: // hh:mm:ss
                var k = s.split(":");
                var d : Date = __new__(Date);                
                d.setTime(0);
                d.setUTCHours(k[0]);
                d.setUTCMinutes(k[1]);
                d.setUTCSeconds(k[2]);                
                result = d;

            case 10: // YYYY-MM-DD
                var k = s.split("-");
                result = new Date(cast k[0],cast k[1] - 1,cast k[2],0,0,0);

            case 19: // YYYY-MM-DD hh:mm:ss
                var k = s.split(" ");
                var y = k[0].split("-");
                var t = k[1].split(":");
                result = new Date(cast y[0],cast y[1] - 1,cast y[2],cast t[0],cast t[1],cast t[2]);

            default:
                throw "Invalid date format : " + s;
            }

        return result;
    }
}
