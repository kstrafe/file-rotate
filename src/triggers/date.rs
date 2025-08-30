use crate::{Action, Trigger};
use std::ops::{BitOr, BitOrAssign};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Time source returning SystemTime. RealTime uses SystemTime::now().
pub trait TimeSource: Clone {
    fn now(&self) -> SystemTime;
}

#[derive(Clone, Copy, Default)]
pub struct RealTime;
impl TimeSource for RealTime {
    fn now(&self) -> SystemTime {
        SystemTime::now()
    }
}

/// Compact set representation for allowed values in a numeric field.
/// Supports ranges up to max < 64. Kept internal as a building block for typed sets below.
#[derive(Clone)]
struct ScheduleSet {
    mask: u64,
    max: u32,
}

impl ScheduleSet {
    fn any(max: u32) -> Self {
        let mask = if max >= 64 {
            u64::MAX
        } else {
            (1u64 << (max + 1)) - 1
        };
        Self { mask, max }
    }
    fn empty(max: u32) -> Self {
        Self { mask: 0, max }
    }
    fn insert(&mut self, v: u32) {
        if v <= self.max {
            self.mask |= 1u64 << v;
        }
    }
    fn contains(&self, v: u32) -> bool {
        (v <= self.max) && (self.mask & (1u64 << v) != 0)
    }
}

#[derive(Clone)]
pub struct MinuteSet(ScheduleSet);
impl MinuteSet {
    pub fn any() -> Self {
        Self(ScheduleSet::any(59))
    }
    pub fn empty() -> Self {
        Self(ScheduleSet::empty(59))
    }
    pub fn exact(v: u32) -> Self {
        let mut s = Self::empty();
        s.0.insert(v.min(59));
        s
    }
    pub fn list(vals: &[u32]) -> Self {
        let mut s = Self::empty();
        for &v in vals {
            s.0.insert(v.min(59));
        }
        s
    }
    pub fn range_step(start: u32, end: u32, step: u32) -> Self {
        let mut s = Self::empty();
        let step = step.max(1);
        let mut v = start;
        while v <= end {
            s.0.insert(v.min(59));
            v = v.saturating_add(step);
        }
        s
    }
    pub fn step_all(step: u32) -> Self {
        Self::range_step(0, 59, step)
    }
    pub fn contains(&self, v: u32) -> bool {
        self.0.contains(v)
    }
}

#[derive(Clone)]
pub struct HourSet(ScheduleSet);
impl HourSet {
    pub fn any() -> Self {
        Self(ScheduleSet::any(23))
    }
    pub fn empty() -> Self {
        Self(ScheduleSet::empty(23))
    }
    pub fn exact(v: u32) -> Self {
        let mut s = Self::empty();
        s.0.insert(v.min(23));
        s
    }
    pub fn list(vals: &[u32]) -> Self {
        let mut s = Self::empty();
        for &v in vals {
            s.0.insert(v.min(23));
        }
        s
    }
    pub fn range_step(start: u32, end: u32, step: u32) -> Self {
        let mut s = Self::empty();
        let step = step.max(1);
        let mut v = start;
        while v <= end {
            s.0.insert(v.min(23));
            v = v.saturating_add(step);
        }
        s
    }
    pub fn step_all(step: u32) -> Self {
        Self::range_step(0, 23, step)
    }
    pub fn contains(&self, v: u32) -> bool {
        self.0.contains(v)
    }
}

#[derive(Clone)]
pub struct DomSet(ScheduleSet); // days 1..=31
impl DomSet {
    pub fn any() -> Self {
        Self(ScheduleSet::any(31))
    }
    pub fn empty() -> Self {
        Self(ScheduleSet::empty(31))
    }
    pub fn exact(v: u32) -> Self {
        let mut s = Self::empty();
        s.0.insert(v.clamp(1, 31));
        s
    }
    pub fn list(vals: &[u32]) -> Self {
        let mut s = Self::empty();
        for &v in vals {
            s.0.insert(v.clamp(1, 31));
        }
        s
    }
    pub fn range_step(start: u32, end: u32, step: u32) -> Self {
        let mut s = Self::empty();
        let step = step.max(1);
        let mut v = start.max(1);
        while v <= end.min(31) {
            s.0.insert(v);
            v = v.saturating_add(step);
        }
        s
    }
    pub fn step_all(step: u32) -> Self {
        Self::range_step(1, 31, step)
    }
    pub fn contains(&self, v: u32) -> bool {
        self.0.contains(v)
    }
}

#[derive(Clone)]
pub struct MonthSet(ScheduleSet); // months 1..=12
impl MonthSet {
    pub fn any() -> Self {
        Self(ScheduleSet::any(12))
    }
    pub fn empty() -> Self {
        Self(ScheduleSet::empty(12))
    }
    pub fn exact(v: u32) -> Self {
        let mut s = Self::empty();
        s.0.insert(v.clamp(1, 12));
        s
    }
    pub fn list(vals: &[u32]) -> Self {
        let mut s = Self::empty();
        for &v in vals {
            s.0.insert(v.clamp(1, 12));
        }
        s
    }
    pub fn range_step(start: u32, end: u32, step: u32) -> Self {
        let mut s = Self::empty();
        let step = step.max(1);
        let mut v = start.max(1);
        while v <= end.min(12) {
            s.0.insert(v);
            v = v.saturating_add(step);
        }
        s
    }
    pub fn step_all(step: u32) -> Self {
        Self::range_step(1, 12, step)
    }
    pub fn contains(&self, v: u32) -> bool {
        self.0.contains(v)
    }
}

/// Bitmask for named days of week with Monday..Sunday mapping to bits 0..6.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DowMask(u8);

impl DowMask {
    pub const MON: DowMask = DowMask(1 << 0);
    pub const TUE: DowMask = DowMask(1 << 1);
    pub const WED: DowMask = DowMask(1 << 2);
    pub const THU: DowMask = DowMask(1 << 3);
    pub const FRI: DowMask = DowMask(1 << 4);
    pub const SAT: DowMask = DowMask(1 << 5);
    pub const SUN: DowMask = DowMask(1 << 6);
    pub const ANY: DowMask = DowMask(0x7F);

    pub fn empty() -> Self {
        DowMask(0)
    }
    pub fn contains(self, mon0_idx: u32) -> bool {
        if mon0_idx > 6 {
            return false;
        }
        (self.0 & (1u8 << mon0_idx)) != 0
    }
}

impl BitOr for DowMask {
    type Output = DowMask;
    fn bitor(self, rhs: DowMask) -> DowMask {
        DowMask(self.0 | rhs.0)
    }
}
impl BitOrAssign for DowMask {
    fn bitor_assign(&mut self, rhs: DowMask) {
        self.0 |= rhs.0;
    }
}

/// A crontab-like date schedule (UTC) with minute/hour/day-of-month/month and named day-of-week mask.
#[derive(Clone)]
pub struct DateSpec {
    pub minute: MinuteSet, // 0..59
    pub hour: HourSet,     // 0..23
    pub dom: DomSet,       // 1..31
    pub month: MonthSet,   // 1..12
    pub dow: DowMask,      // Monday..Sunday bits 0..6
}

impl Default for DateSpec {
    fn default() -> Self {
        Self {
            minute: MinuteSet::any(),
            hour: HourSet::any(),
            dom: DomSet::any(),
            month: MonthSet::any(),
            dow: DowMask::ANY,
        }
    }
}

impl DateSpec {
    pub fn new() -> Self {
        Self::default()
    }

    /// Convenience: rotate every n days at given hour:minute (UTC).
    /// Interpreted as DOM */n starting at day 1.
    pub fn every_n_days_at(hour: u32, minute: u32, n: u32) -> Self {
        Self {
            minute: MinuteSet::exact(minute.min(59)),
            hour: HourSet::exact(hour.min(23)),
            // DOM range is 1..=31; implement */n as 1, 1+n, 1+2n, ...
            dom: DomSet::range_step(1, 31, n.max(1)),
            ..Default::default()
        }
    }

    /// Set the days of week mask (Mon..Sun). Example: DowMask::MON | DowMask::WED | DowMask::FRI
    pub fn with_dow(mut self, mask: DowMask) -> Self {
        self.dow = mask;
        self
    }
}

/// Rotate whenever the current UTC time matches a DateSpec (once per matching minute).
pub struct Date<TS: TimeSource> {
    spec: DateSpec,
    time: TS,
    last_fired_minute: Option<u64>,
    observed: Option<SystemTime>,
}

impl<TS: TimeSource> Date<TS> {
    pub fn new(time: TS, spec: DateSpec) -> Self {
        Self {
            spec,
            time,
            last_fired_minute: None,
            observed: None,
        }
    }

    fn dur_from_system_time(t: SystemTime) -> Duration {
        t.duration_since(UNIX_EPOCH)
            .unwrap_or_else(|e| e.duration())
    }

    // Howard Hinnant's civil_from_days algorithm
    fn ymd_from_days(mut z: i64) -> (i32, u32, u32) {
        z += 719_468; // shift to civil epoch
        let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
        let doe = z - era * 146_097; // [0, 146096]
        let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365; // [0,399]
        let y = yoe + era * 400;
        let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        let mp = (5 * doy + 2) / 153; // [0,11]
        let d = doy - (153 * mp + 2) / 5 + 1; // [1,31]
        let m = mp + if mp < 10 { 3 } else { -9 }; // [1,12]
        let y = y + if m <= 2 { 1 } else { 0 };
        (y as i32, m as u32, d as u32)
    }

    fn components_utc(ts: SystemTime) -> (u32, u32, u32, u32, u32, u64) {
        let secs = Self::dur_from_system_time(ts).as_secs();
        let minute = ((secs / 60) % 60) as u32;
        let hour = ((secs / 3600) % 24) as u32;
        let days = (secs / 86_400) as i64;
        let (_year, month, dom) = Self::ymd_from_days(days);
        // 1970-01-01 was Thursday. With Sunday=0, Thursday=4.
        let dow_sun0 = ((days.rem_euclid(7) + 4) % 7) as u32; // 0=Sun..6=Sat
        let dow_mon0 = (dow_sun0 + 6) % 7; // 0=Mon..6=Sun
        let minute_epoch = secs / 60; // used to dedupe triggers within the same minute
        (minute, hour, dom, month, dow_mon0, minute_epoch)
    }
}

impl<TS: TimeSource> Trigger for Date<TS> {
    type Meta = SystemTime;

    fn trigger(&mut self, bytes: &[u8]) -> Action {
        if bytes.is_empty() {
            return Action::None;
        }
        let now = self.observed.take().unwrap_or_else(|| self.time.now());
        let (min, hr, dom, mon, dow_mon0, minute_epoch) = Self::components_utc(now);
        let spec = &self.spec;
        if spec.minute.contains(min)
            && spec.hour.contains(hr)
            && spec.dom.contains(dom)
            && spec.month.contains(mon)
            && spec.dow.contains(dow_mon0)
        {
            if self.last_fired_minute == Some(minute_epoch) {
                Action::None
            } else {
                Action::Rotate { consumed: 0 }
            }
        } else {
            Action::None
        }
    }

    fn reset(&mut self) {
        let now = self.observed.take().unwrap_or_else(|| self.time.now());
        let (_min, _hr, _dom, _mon, _dow, minute_epoch) = Self::components_utc(now);
        self.last_fired_minute = Some(minute_epoch);
    }

    fn observe(&mut self, _bytes: &[u8]) -> Self::Meta {
        let now = self.time.now();
        self.observed = Some(now);
        now
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Default)]
    struct MockTime {
        secs: u64,
    }
    impl MockTime {
        fn set(&mut self, secs: u64) {
            self.secs = secs;
        }
    }
    impl TimeSource for MockTime {
        fn now(&self) -> SystemTime {
            UNIX_EPOCH + Duration::from_secs(self.secs)
        }
    }

    #[test]
    fn matches_every_2_days_at_13_00() {
        let mut clock = MockTime::default();
        let spec = DateSpec::every_n_days_at(13, 0, 2);
        let mut t = Date::new(clock.clone(), spec);
        // Initialize
        assert!(matches!(t.trigger(b"init"), Action::None));
        // Day 0 at 13:00 should match
        clock.set(13 * 3600);
        t.time = clock.clone();
        match t.trigger(b"x") {
            Action::Rotate { consumed } => assert_eq!(consumed, 0),
            _ => panic!("expected rotate"),
        }
        t.reset();
        // Day 1 at 13:00 should NOT match (*/2 DOM)
        clock.set(86_400 + 13 * 3600);
        t.time = clock.clone();
        assert!(matches!(t.trigger(b"y"), Action::None));
        // Day 2 at 12:59 no match
        clock.set(2 * 86_400 + 12 * 3600 + 59 * 60);
        t.time = clock.clone();
        assert!(matches!(t.trigger(b"z"), Action::None));
        // Day 2 at 13:00 match
        clock.set(2 * 86_400 + 13 * 3600);
        t.time = clock.clone();
        match t.trigger(b"w") {
            Action::Rotate { consumed } => assert_eq!(consumed, 0),
            _ => panic!("expected rotate"),
        }
    }

    #[test]
    fn observe_returns_now_systemtime() {
        let mut clock = MockTime::default();
        let mut t = Date::new(clock.clone(), DateSpec::new());
        clock.set(12_345);
        t.time = clock.clone();
        let m = t.observe(b"");
        assert_eq!(m.duration_since(UNIX_EPOCH).unwrap().as_secs(), 12_345);
    }

    #[test]
    fn named_weekdays_mask_works() {
        // 1970-01-01 is Thursday; check mapping to Mon..Sun bits
        let mut clock = MockTime::default();
        let spec = DateSpec {
            minute: MinuteSet::exact(0),
            hour: HourSet::exact(0),
            dom: DomSet::any(),
            month: MonthSet::any(),
            dow: DowMask::THU | DowMask::FRI, // allow Thu and Fri only
        };
        let mut t = Date::new(clock.clone(), spec);
        // Thu 1970-01-01 00:00 -> should rotate at 00:00
        clock.set(0);
        t.time = clock.clone();
        match t.trigger(b"start") {
            Action::Rotate { .. } => {}
            _ => panic!("expected rotate on Thu 00:00"),
        }
        t.reset();
        // Fri 1970-01-02 00:00 -> should rotate
        clock.set(86_400);
        t.time = clock.clone();
        match t.trigger(b"fri") {
            Action::Rotate { .. } => {}
            _ => panic!("expected rotate on Fri 00:00"),
        }
        t.reset();
        // Sat should NOT rotate
        clock.set(2 * 86_400);
        t.time = clock.clone();
        assert!(matches!(t.trigger(b"sat"), Action::None));
    }
}
