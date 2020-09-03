package lua.lib.lpeg;
import lua.lib.lpeg.Pattern;

class Rehx {
    public static function compile(str : String, ?def : Dynamic) : Pattern {
        return null;
    }

    static var I =  Pattern.P((s:String,i:Int)-> lua.Lua.print(i, s.substring(0,i)));

    static function getDef(id:Dynamic, defs : Table<String, String>){
        if (defs[id] != null) {
            return defs[id];
        } else {
            throw ('undefined name: ${id}');
            return null;
        }
    }

    static function patError(s:String, i:Int){
        var sub = if (s.length <  (i + 20)){
            s.substring(i);
        } else {
            s.substring(i, i+20) + "...";
        }
        throw('pattern error near $sub');
    }

    static function mult(p : Pattern, n : Int){
        var np = Pattern.P(true);
        while (n >= 1){
            if (n%2 >=1) np = np + p;
            p = p + p;
            n = Std.int(n/2);
        }
        return np;
    }

    static function equalCap(s:String, i:Int, c:String) : Int {
        if (lua.Lua.type(c)  != "string") return null;
        var e = c.length + i;
        if (s.substring(i, e-1) == c) {
            return e;
        } else return null;

    }


}
