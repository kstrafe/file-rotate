// Shared test utilities compiled only for tests.

use std::cell::Cell;
use std::rc::Rc;
use std::time::Duration;

use crate::triggers::interval::Clock;

pub mod time {
    use super::*;

    // A simple clock with second granularity and an interior-mutable counter.
    pub struct MockClock {
        t: Cell<u64>,
    }

    impl Default for MockClock {
        fn default() -> Self {
            Self::new()
        }
    }

    impl MockClock {
        pub fn new() -> Self {
            Self { t: Cell::new(0) }
        }
        pub fn set(&self, secs: u64) {
            self.t.set(secs);
        }
    }

    impl Clock for MockClock {
        fn now(&self) -> Duration {
            Duration::from_secs(self.t.get())
        }
    }

    // A clonable step clock driven by an Rc<Cell<Duration>> so multiple sites can advance time.
    #[derive(Clone)]
    pub struct StepClock(pub Rc<Cell<Duration>>);

    impl StepClock {
        pub fn new(cell: Rc<Cell<Duration>>) -> Self {
            StepClock(cell)
        }
    }

    impl Clock for StepClock {
        fn now(&self) -> Duration {
            self.0.get()
        }
    }
}
