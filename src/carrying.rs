pub(crate) trait CarryingExt {
    fn add_carrying(self, rhs: Self, carry: bool) -> (Self, bool)
    where
        Self: Sized;
    fn sub_borrowing(self, rhs: Self, borrow: bool) -> (Self, bool)
    where
        Self: Sized;
}

macro_rules! impl_carrying {
    ($($SelfT: ty)*) => {
        $(impl CarryingExt for $SelfT {
            #[inline]
            fn add_carrying(self, rhs: Self, carry: bool) -> (Self, bool) {
                let (a, b) = self.overflowing_add(rhs);
                let (c, d) = a.overflowing_add(carry as $SelfT);
                (c, b != d)
            }

            #[inline]
            fn sub_borrowing(self, rhs: Self, borrow: bool) -> (Self, bool) {
                let (a, b) = self.overflowing_sub(rhs);
                let (c, d) = a.overflowing_sub(borrow as $SelfT);
                (c, b != d)
            }
        })*
    };
}

impl_carrying! {
    u8
    i8
}
