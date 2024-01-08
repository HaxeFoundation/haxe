function main() {
	Sys.setCwd("./project");
	Sys.command("javac", ["-d", "out", "test/Robot.java", "test/RobotFactory.java", "-g"]);
	Sys.setCwd("./out");
	Sys.command("jar", ["cf", "test.jar", "test/Robot.class", "test/Robot$CleanupTask.class", "test/Robot$MathOperation.class", "test/Robot$GreetRobot.class", "test/Robot$ManufactureRobot.class", "test/RobotFactory.class", "test/RobotFactory$1.class", "test/RobotFactory$2.class", "test/RobotFactory$3.class"]);
}
