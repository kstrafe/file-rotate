//! Triggers decide when a rotation should occur.
//!
//! A trigger inspects the bytes being written and returns an `Action`, either
//! requesting a rotation and indicating how many bytes were consumed before the
//! rotation point, or `Action::None` to continue writing.
//!
//! Triggers are stateful and must implement `reset`, which is called by
//! `FileRotate` after a rotation. When triggers are combined with the
//! combinator, both triggers are reset whenever a rotation occurs.
pub mod bytes;
pub mod combinators;
pub mod delimiter;
pub mod interval;
pub mod line_count;

pub use bytes::Bytes;
pub use combinators::{TriggerCombinator, TriggerExt};
pub use delimiter::Delimiter;
pub use interval::{Clock, Interval, RealClock};
pub use line_count::LineCount;
