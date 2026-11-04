final class FinalDisplayTest implements FinalDisplay {} // here it works fine
final class FinalDisplayTestFailed implements FinalDisplay {} // seconds class shows: class needs to be final diagnostics(2)

function main() {}