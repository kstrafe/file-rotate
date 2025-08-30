use crate::{Action, Trigger};

/// Trigger combinator that allows OR logic between triggers.
///
/// Evaluates the first trigger; if it requests a rotation, that result is
/// returned. Otherwise evaluates the second trigger. When a rotation occurs,
/// `reset` is called on the combinator which resets both underlying triggers.
pub struct TriggerCombinator<A, B> {
    pub(crate) trigger_a: A,
    pub(crate) trigger_b: B,
}

impl<A, B> TriggerCombinator<A, B>
where
    A: Trigger,
    B: Trigger,
{
    pub fn new(trigger_a: A, trigger_b: B) -> Self {
        Self {
            trigger_a,
            trigger_b,
        }
    }
}

impl<A, B> Trigger for TriggerCombinator<A, B>
where
    A: Trigger,
    B: Trigger,
{
    type Meta = (A::Meta, B::Meta);

    fn trigger(&mut self, bytes: &[u8]) -> Action {
        match self.trigger_a.trigger(bytes) {
            Action::Rotate { consumed } => Action::Rotate { consumed },
            Action::None => self.trigger_b.trigger(bytes),
        }
    }

    fn reset(&mut self) {
        self.trigger_a.reset();
        self.trigger_b.reset();
    }

    fn observe(&mut self, bytes: &[u8]) -> Self::Meta {
        // Ensure both children see the bytes and combine their metadata.
        let a = self.trigger_a.observe(bytes);
        let b = self.trigger_b.observe(bytes);
        (a, b)
    }
}

/// Extension trait to add `.or()` method to triggers.
///
/// This provides a convenient way to combine two triggers with OR semantics.
pub trait TriggerExt: Trigger + Sized {
    fn or<T: Trigger>(self, other: T) -> TriggerCombinator<Self, T> {
        TriggerCombinator::new(self, other)
    }
}

impl<T: Trigger> TriggerExt for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Bytes;

    fn drive(mut trig: impl Trigger, mut buf: &[u8]) -> Vec<usize> {
        // Returns a list of consumed lengths for each rotation; calls reset between rotations.
        let mut consumed_list = Vec::new();
        while let Action::Rotate { consumed } = trig.trigger(buf) {
            consumed_list.push(consumed);
            trig.reset();
            // Avoid infinite loop on zero-consumed rotations
            if consumed == 0 {
                break;
            }
            buf = &buf[consumed..];
            if buf.is_empty() {
                break;
            }
        }
        consumed_list
    }

    #[test]
    fn nested_or_resets_all_children() {
        let a = Bytes::new().limit(100); // won't fire
        let b = Bytes::new().limit(3); // fires on first 3
        let c = Bytes::new().limit(2); // fires on next 2 after reset

        let combo = a.or(b.or(c));
        let rotates = drive(combo, b"abcde");
        assert_eq!(rotates, vec![3, 2]);
    }

    #[test]
    fn multiple_rotations_across_calls() {
        let combo = Bytes::new().limit(2).or(Bytes::new().limit(3));
        let mut trig = combo;
        // First buffer triggers first arm
        match trig.trigger(b"ab") {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected rotate"),
        }
        trig.reset();
        // Second buffer triggers second arm
        // With left-biased OR, the first arm (limit 2) still fires
        match trig.trigger(b"cde") {
            Action::Rotate { consumed } => assert_eq!(consumed, 2),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn zero_limit_edge_triggers_immediately() {
        let combo = Bytes::new().limit(0).or(Bytes::new().limit(1));
        let rotates = drive(combo, b"xyz");
        // Immediate rotate with 0 consumed (first arm), then after reset next call would be empty so we stop
        assert_eq!(rotates, vec![0]);
    }

    #[test]
    fn one_limit_consumes_one_then_stops() {
        let combo = Bytes::new().limit(1).or(Bytes::new().limit(100));
        let rotates = drive(combo, b"hello");
        assert_eq!(rotates, vec![1, 1, 1, 1, 1]);
    }

    #[test]
    fn nested_mixed_delimiter_and_bytes() {
        use crate::Delimiter;
        // Inner: rotate on newline (include), or after 4 bytes
        let inner = Delimiter::new(b"\n", true).or(Bytes::new().limit(4));
        // Outer with a small bytes trigger to verify reset behavior of both arms
        let combo = inner.or(Bytes::new().limit(2));

        // Drive with data that first hits outer (2 bytes), then newline, then 4 bytes
        // But OR is left-biased, so inner (left) will fire first on the newline
        // 1) inner Delimiter consumes up to and including newline => 3
        // 2) remaining has at least 4 bytes -> inner Bytes(4) fires at 4
        let rotates = drive(combo, b"ab\nxyzqt");
        assert_eq!(rotates, vec![3, 4]);
    }

    #[test]
    fn observe_is_dispatched_to_both_children() {
        struct Probe {
            seen: usize,
        }
        impl Trigger for Probe {
            type Meta = ();
            fn trigger(&mut self, _bytes: &[u8]) -> Action {
                Action::None
            }
            fn reset(&mut self) {}
            fn observe(&mut self, bytes: &[u8]) -> Self::Meta {
                self.seen += bytes.len();
            }
        }
        let mut combo = TriggerCombinator::new(Probe { seen: 0 }, Probe { seen: 0 });
        let _ = combo.observe(b"hello");
        // Access internal fields to assert both were updated
        assert_eq!(combo.trigger_a.seen, 5);
        assert_eq!(combo.trigger_b.seen, 5);
    }
}
