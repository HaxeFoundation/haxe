function UpperCaseMod() {
    return "ModWithStaticAndClassStatic.UpperCaseMod";
}

function lowerCaseMod() {
    return "ModWithStaticAndClassStatic.lowerCaseMod";
}

class ModWithStaticAndClassStatic {
    public static function UpperCaseMod() {
        return "ModWithStaticAndClassStatic.ModWithStaticAndClassStatic.UpperCaseMod";
    }
    
    public static function lowerCaseMod() {
        return "ModWithStaticAndClassStatic.ModWithStaticAndClassStatic.lowerCaseMod";
    }
}
