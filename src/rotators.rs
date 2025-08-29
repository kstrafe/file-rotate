//! Rotators decide how a rotation is performed.
//!
//! Given a rotation decision from a trigger, a rotator is responsible for
//! moving, renaming, or otherwise handling the current destination and creating
//! a new writer to continue writing to. Rotators encapsulate filesystem
//! semantics or in-memory behavior and surface IO errors directly.
pub mod dated_suffix;
pub mod memory;
pub mod numbered_suffix;

pub use dated_suffix::DatedSuffix;
pub use memory::{MemoryRotator, MemoryWriter};
pub use numbered_suffix::NumberedSuffix;
