use file_rotate::{
    rotators::MemoryRotator,
    triggers::{Clock, Interval},
    FileRotate,
};
use std::cell::Cell;
use std::io::Write;
use std::time::Duration;

#[derive(Clone)]
struct StepClock(Cell<Duration>);
impl Clock for StepClock {
    fn now(&self) -> Duration {
        self.0.get()
    }
}

fn main() -> std::io::Result<()> {
    let clock = StepClock(Cell::new(Duration::from_secs(0)));
    let trigger = Interval::new(clock.clone(), Duration::from_secs(1));
    let rotator = MemoryRotator::new();
    let mut log = FileRotate::new(rotator, trigger)?;

    write!(log, "hello")?;
    // Advance time to exceed the interval, causing rotation on the next write.
    clock.0.set(Duration::from_secs(2));
    write!(log, "world")?;

    Ok(())
}
