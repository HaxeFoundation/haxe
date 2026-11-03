@:genericBuild(Macro.build())
private class Foo<Rest> {}
private typedef Bar = Foo<Int, default>

function main() {}
