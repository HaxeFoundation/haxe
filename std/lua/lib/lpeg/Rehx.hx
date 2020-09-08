package lua.lib.lpeg;
import lua.lib.lpeg.Pattern;
import lua.lib.lpeg.Pattern.*;
import lua.lib.lpeg.Re;
import lua.lib.lpeg.Re.S;

class Rehx {
    public static function compile(str : String, ?def : Dynamic) : Pattern {
        return null;
    }
    public static function match(subject : String, pattern : Pattern, ?index : Int) : String {
        return Re.match(subject, pattern, index);
    }
    public static function find(subject : String, pattern : Pattern, ?index : Int) : Int {
        return Re.find(subject, pattern, index);
    }
    public static function gsub(subject : String, pattern : Pattern, replace : String) : String {
        return Re.gsub(subject, pattern, replace);
    }
    public static function updatelocale() : Void {
        return Re.updatelocale();
    }

    var  exp = P([
        "__name__" => "Exp",
        "Exp" => S + ( V("Grammar") | Cf(V("Seq") + ("/" + S + V("Seq")) * 0, mt.__add) ),
        "Seq" => Cf(Cc(P("")) * V("Prefix")^0 , mt.__mul) * (seq_follow.length + patt_error),
        "Prefix" => "&" * S * V("Prefix") / mt.__len
            + "!" * S * V("Prefix") / mt.__unm
            + V("Suffix"),
        "Suffix" => Cf(V("Primary") * S *
          ( ( P("+") * Cc(1, mt.__pow)
            + P("*") * Cc(0, mt.__pow)
            + P("?") * Cc(-1, mt.__pow)
            + "^" * ( Cg(num * m.Cc(mult))
                    + Cg(C(S("+-") * R("09")^1) * Cc(mt.__pow))
                    )
            + "->" * S * ( Cg((String + num) * Cc(mt.__div))
                         + P("{}") * Cc(nil, Ct)
                         + Cg(Def / getdef * Cc(mt.__div))
                         )
            + "=>" * S * Cg(Def / getdef * Cc(Cmt))
            ) * S
          )^0, function (a,b,f) return f(a,b) ),
        "Primary" => "(" * V("Exp") * ")"
            + String / P
            + Class
            + defined
            + "{:" * (name * ":" + Cc(null)) * V("Exp") * ":}" /
                     function (n, p) return Cg(p, n)
            + "=" * name / function (n) return Cmt(Cb(n), equalcap)
            + P("{}") / Cp
            + "{~" * m.V("Exp") * "~}" / Cs
            + "{|" * m.V("Exp") * "|}" / Ct
            + "{" * m.V("Exp") * "}" / C
            + P(".") * Cc(any)
            + (name * -arrow + "<" * name * ">") * Cb("G") / NT,
        "Definition" => name * arrow * V("Exp"),
        "Grammar" => Cg(Cc(true), "G") *
            Cf(V("Definition") / firstdef * Cg(V("Definition"))^0,
              adddef) / P
    ]);
}
