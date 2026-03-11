import haxe.ds.Option;

class Main {
	public static void main(String[] args) {
		Option option = Option.Some("test");
		if (option instanceof Option.Some) {
			if (((Option.Some) option).v.equals("test")) {
				System.exit(0);
			}
		}
		System.out.println("Failed to match Some(\"test\").");
		System.exit(1);
	}
}
