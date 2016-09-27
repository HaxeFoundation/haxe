#if !macro
@:build(Main.build1())
@:build(Main.build2())
#end
class Main {
    #if macro
    static function build1() {
        Sys.stderr().writeString("Build 1");
        return haxe.macro.Context.getBuildFields();
    }

    static function build2() {
        Sys.stderr().writeString("Build 2");
        return haxe.macro.Context.getBuildFields();
    }
    #end

    static function main() {
    }
}