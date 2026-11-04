package test;

public abstract class Robot<T> {
    public Robot() {}

    public void performTask(T listener) {
        System.out.println("Robot.performTask() called!");
    }

    public void performTask(T listener, CleanupTask cleanupTask) {
        System.out.println("Robot.performTask() called!");
        cleanupTask.cleanup();
    }

    /**
     * MathOperation
     */
    @FunctionalInterface
    public interface MathOperation {
        public int operate(int a, int b);
    }

    @FunctionalInterface
    public interface GreetRobot {
        public void greet(Robot robot);
    }

    @FunctionalInterface
    public interface ManufactureRobot<T extends Robot> {
        public T manufacture(String robotType);
    }

    @FunctionalInterface
    public interface CleanupTask {
        public void cleanup();
    }

    @Override
    public String toString() {
        return "Robot";
    }
}
