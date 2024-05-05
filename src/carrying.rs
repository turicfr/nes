pub(crate) trait CarryingExt {
    fn add_carrying(self, rhs: Self, carry: bool) -> (Self, bool) where Self: Sized;
}

impl CarryingExt for u8 {
    #[inline]
    fn add_carrying(self, rhs: Self, carry: bool) -> (Self, bool) {
        let (a, b) = self.overflowing_add(rhs);
        let (c, d) = a.overflowing_add(carry as u8);
        (c, b != d)
    }
}

impl CarryingExt for i8 {
    #[inline]
    fn add_carrying(self, rhs: Self, carry: bool) -> (Self, bool) {
        let (a, b) = self.overflowing_add(rhs);
        let (c, d) = a.overflowing_add(carry as i8);
        (c, b != d)
    }
}
