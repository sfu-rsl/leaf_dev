macro_rules! define_reversible_pair {
    ($(#[$attr: ident($($attr_params: tt)*)])* $name: ident $(<$($generic_type: ident),*>)? { ($orig: ident, $rev: ident) { $first: ident : $t_first: ty, $second: ident : $t_second: ty $(,)?} } $(,)? $($(#[$impl_attr: ident($($impl_attr_params: tt)*)])* impl)?) => {
        $(#[$attr($($attr_params)*)])*
        pub(crate) enum $name$(<$($generic_type),*>)? {
            $orig {
                $first: $t_first,
                $second: $t_second,
            },
            $rev {
                $first: $t_second,
                $second: $t_first,
            },
        }

        $($(#[$impl_attr($($impl_attr_params)*)])*)?
        impl$(<$($generic_type),*>)? $name$(<$($generic_type),*>)? {
            #[inline]
            pub fn flatten(self) -> ($t_first, $t_second, bool) {
                match self {
                    Self::$orig { $first, $second } => ($first, $second, false),
                    Self::$rev { $second, $first } => ($second, $first, true),
                }
            }

            #[inline]
            pub fn as_flat(&self) -> (&$t_first, &$t_second, bool) {
                match self {
                    Self::$orig { $first, $second } => ($first, $second, false),
                    Self::$rev { $second, $first } => ($second, $first, true),
                }
            }
        }

        impl$(<$($generic_type),*>)? From<($t_first, $t_second)> for $name$(<$($generic_type),*>)? {
            fn from(pair: ($t_first, $t_second)) -> Self {
                Self::$orig {
                    $first: pair.0,
                    $second: pair.1,
                }
            }
        }

        impl$(<$($generic_type),*>)? From<($t_first, $t_second, bool)> for $name$(<$($generic_type),*>)? {
            fn from(pair: ($t_first, $t_second, bool)) -> Self {
                if !pair.2 {
                    Self::$orig {
                        $first: pair.0,
                        $second: pair.1,
                    }
                } else {
                    Self::$rev {
                        $first: pair.1,
                        $second: pair.0,
                    }
                }
            }
        }
    };
}

pub(crate) use define_reversible_pair;
