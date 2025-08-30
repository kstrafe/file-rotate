use crate::{Action, Trigger};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// A clock trait to make `Interval` trigger testable and deterministic.
///
/// Provide your own implementation in tests to control the flow of time.
pub trait Clock {
    fn now(&self) -> Duration;
}

/// Real clock using system time.
pub struct RealClock;
impl Clock for RealClock {
    fn now(&self) -> Duration {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_else(|_| Duration::from_secs(0))
    }
}

/// Rotate when a period has elapsed since the last rotation (or initialization).
///
/// The first non-empty write initializes the reference time. Subsequent writes
/// will request a rotation if the configured `period` has elapsed.
pub struct Interval<C: Clock> {
    period: Duration,
    last: Option<Duration>,
    clock: C,
    observed: Option<Duration>,
}

impl<C: Clock> Interval<C> {
    /// Create a new `Interval` trigger using the given clock and period.
    pub fn new(clock: C, period: Duration) -> Self {
        Self {
            period,
            last: None,
            clock,
            observed: None,
        }
    }
}

impl<C: Clock> Trigger for Interval<C> {
    type Meta = Duration;

    fn trigger(&mut self, bytes: &[u8]) -> Action {
        if bytes.is_empty() {
            return Action::None;
        }
        // Use the most recent observed time if available; otherwise sample now.
        let now = self.observed.take().unwrap_or_else(|| self.clock.now());
        match self.last {
            None => {
                self.last = Some(now);
                Action::None
            }
            Some(last) => {
                if now.checked_sub(last).unwrap_or_default() >= self.period {
                    Action::Rotate { consumed: 0 }
                } else {
                    Action::None
                }
            }
        }
    }

    fn reset(&mut self) {
        // Advance deterministically: if initialized, move the reference forward by one period
        // without sampling the clock. If uninitialized, fall back to observed/now.
        let new_last = match self.last {
            Some(last) => last + self.period,
            None => self.observed.take().unwrap_or_else(|| self.clock.now()),
        };
        self.last = Some(new_last);
    }

    fn observe(&mut self, _bytes: &[u8]) -> Self::Meta {
        // Capture the current time as the observation for subsequent trigger.
        let now = self.clock.now();
        self.observed = Some(now);
        now
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::time::MockClock;

    #[test]
    fn first_non_empty_initializes_then_rotates_after_period() {
        let clock = MockClock::new();
        let mut t = Interval::new(clock, Duration::from_secs(5));
        assert!(matches!(t.trigger(b"x"), Action::None));
        // advance to just before period
        t.clock.set(4);
        assert!(matches!(t.trigger(b"y"), Action::None));
        // advance to period boundary
        t.clock.set(5);
        match t.trigger(b"z") {
            Action::Rotate { consumed } => assert_eq!(consumed, 0),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn empty_slice_never_triggers() {
        let clock = MockClock::new();
        let mut t = Interval::new(clock, Duration::from_secs(1));
        assert!(matches!(t.trigger(b""), Action::None));
    }

    #[test]
    fn reset_sets_last_to_now() {
        let clock = MockClock::new();
        let mut t = Interval::new(clock, Duration::from_secs(10));
        assert!(matches!(t.trigger(b"init"), Action::None));
        t.clock.set(9);
        t.reset();
        // Without advancing, should not rotate immediately
        assert!(matches!(t.trigger(b"x"), Action::None));
    }

    #[test]
    fn observe_time_is_used_by_trigger() {
        // period = 5
        let clock = MockClock::new();
        let mut t = Interval::new(clock, Duration::from_secs(5));
        // Initialize last at t=0
        assert!(matches!(t.trigger(b"init"), Action::None));
        // Observe at time 4
        t.clock.set(4);
        let observed = t.observe(b"data");
        assert_eq!(observed, Duration::from_secs(4));
        // Advance time beyond period
        t.clock.set(6);
        // Trigger should use observed (4), so not rotate yet because 4-0 < 5
        assert!(matches!(t.trigger(b"x"), Action::None));
    }

    #[test]
    fn reset_adds_period_and_is_deterministic() {
        // period = 5
        let clock = MockClock::new();
        let mut t = Interval::new(clock, Duration::from_secs(5));
        // Initialize at t=0
        assert!(matches!(t.trigger(b"init"), Action::None));

        // Reset should advance last to 5 without sampling the clock
        t.reset();
        t.clock.set(4);
        assert!(matches!(t.trigger(b"no"), Action::None)); // 4 - 5 < 5
                                                           // At boundary (now == last), should NOT rotate yet
        t.clock.set(5);
        assert!(matches!(t.trigger(b"boundary"), Action::None));
        // After a full additional period (now == 10), should rotate
        t.clock.set(10);
        match t.trigger(b"yes") {
            Action::Rotate { consumed } => assert_eq!(consumed, 0),
            _ => panic!("expected rotate at t=10 after reset"),
        }

        // Jump far ahead; reset should NOT anchor to now, so immediate rotate is possible
        t.clock.set(100);
        t.reset(); // last becomes previous_last + 5, not 100
        match t.trigger(b"immediate") {
            Action::Rotate { .. } => {}
            _ => panic!("expected immediate rotate since now - last >= period"),
        }
    }
}
