#if macro
import haxe.macro.Expr;
#end

@:forward(x)
abstract Vec2(Vec2Data) to Vec2Data from Vec2Data {
    public inline function new(x: Float) {
        this = new Vec2Data(x);
    }

    @:op(a.b) macro function swizzleRead(self, name: String): ExprOf<Vec2> {
        return macro {
            var self = $self;
            new Vec2(self.x);
        }
    }

    @:op(a.b) macro function swizzleWrite(self, name: String, value: Expr): ExprOf<Vec2> {
        return macro ({
            var self = $self;
            var value: Vec2 = $value;
            self.x = value.x;
            value;
        }: Vec2);
    }

    @:op(a * b) @:commutative
    static inline function mulScalar(a: Vec2, b: Float): Vec2
        return new Vec2(a.x * b);
}

private class Vec2Data {
    public var x: Float;
    public inline function new(x: Float) {
        this.x = x;
    }
}

class Main {
    static function main() {
        var color = new Vec2(1.0);
        color.xy *= 0.5;
    }
}
