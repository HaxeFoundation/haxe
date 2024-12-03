package test;

import test.Robot.GreetRobot;
import test.Robot.ManufactureRobot;
import test.Robot.MathOperation;

public class RobotFactory {
    public static Robot<MathOperation> buildMathRobot() {
        return new Robot<MathOperation>() {
            public void performTask(MathOperation listener) {
                System.out.println("Robot.performTask() called!");
                int result = listener.operate(3, 4);
                System.out.println("Result: " + result);
            }

            public void performTask(MathOperation listener, CleanupTask cleanupTask) {
                System.out.println("Robot.performTask() called!");
                int result = listener.operate(3, 4);
                System.out.println("Result: " + result);
                cleanupTask.cleanup();
            }
        };
    }

    public static Robot<GreetRobot> buildGreetRobot(Robot target) {
        return new Robot<GreetRobot>() {
            public void performTask(GreetRobot listener) {
                System.out.println("Robot.performTask() called!");
                listener.greet(target);
            }

            public void performTask(GreetRobot listener, CleanupTask cleanupTask) {
                System.out.println("Robot.performTask() called!");
                listener.greet(target);
                cleanupTask.cleanup();
            }
        };
    }

    public static Robot<ManufactureRobot<Robot<GreetRobot>>> buildManufactureRobot() {
        return new Robot<ManufactureRobot<Robot<GreetRobot>>>() {
            public void performTask(ManufactureRobot<Robot<GreetRobot>> listener) {
                System.out.println("Robot.performTask() called!");
                Robot<GreetRobot> output = listener.manufacture("Greet");
                System.out.println("Output: " + output.toString());
            }

            public void performTask(ManufactureRobot<Robot<GreetRobot>> listener, CleanupTask cleanupTask) {
                System.out.println("Robot.performTask() called!");
                Robot<GreetRobot> output = listener.manufacture("Greet");
                System.out.println("Output: " + output.toString());
                cleanupTask.cleanup();
            }
        };
    }
}
