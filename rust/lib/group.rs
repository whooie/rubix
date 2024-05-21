//! Rubik's cube group abstractions.

type Perm = Vec<usize>;

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Move {
    F,  Fp,  F2,
    B,  Bp,  B2,
    L,  Lp,  L2,
    R,  Rp,  R2,
    U,  Up,  U2,
    D,  Dp,  D2,
    f,  fp,  f2,
    b,  bp,  b2,
    l,  lp,  l2,
    r,  rp,  r2,
    u,  up,  u2,
    d,  dp,  d2,
    M,  Mp,  M2,
    E,  Ep,  E2,
    S,  Sp,  S2,
    X,  Xp,  X2,
    Y,  Yp,  Y2,
    Z,  Zp,  Z2,
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::F  => write!(f, "F"),
            Self::Fp => write!(f, "F'"),
            Self::F2 => write!(f, "F2"),
            Self::B  => write!(f, "B"),
            Self::Bp => write!(f, "B'"),
            Self::B2 => write!(f, "B2"),
            Self::L  => write!(f, "L"),
            Self::Lp => write!(f, "L'"),
            Self::L2 => write!(f, "L2"),
            Self::R  => write!(f, "R"),
            Self::Rp => write!(f, "R'"),
            Self::R2 => write!(f, "R2"),
            Self::U  => write!(f, "U"),
            Self::Up => write!(f, "U'"),
            Self::U2 => write!(f, "U2"),
            Self::D  => write!(f, "D"),
            Self::Dp => write!(f, "D'"),
            Self::D2 => write!(f, "D2"),
            Self::f  => write!(f, "f"),
            Self::fp => write!(f, "f'"),
            Self::f2 => write!(f, "f2"),
            Self::b  => write!(f, "b"),
            Self::bp => write!(f, "b'"),
            Self::b2 => write!(f, "b2"),
            Self::l  => write!(f, "l"),
            Self::lp => write!(f, "l'"),
            Self::l2 => write!(f, "l2"),
            Self::r  => write!(f, "r"),
            Self::rp => write!(f, "r'"),
            Self::r2 => write!(f, "r2"),
            Self::u  => write!(f, "u"),
            Self::up => write!(f, "u'"),
            Self::u2 => write!(f, "u2"),
            Self::d  => write!(f, "d"),
            Self::dp => write!(f, "d'"),
            Self::d2 => write!(f, "d2"),
            Self::M  => write!(f, "M"),
            Self::Mp => write!(f, "M'"),
            Self::M2 => write!(f, "M2"),
            Self::E  => write!(f, "E"),
            Self::Ep => write!(f, "E'"),
            Self::E2 => write!(f, "E2"),
            Self::S  => write!(f, "S"),
            Self::Sp => write!(f, "S'"),
            Self::S2 => write!(f, "S2"),
            Self::X  => write!(f, "X"),
            Self::Xp => write!(f, "X'"),
            Self::X2 => write!(f, "X2"),
            Self::Y  => write!(f, "Y"),
            Self::Yp => write!(f, "Y'"),
            Self::Y2 => write!(f, "Y2"),
            Self::Z  => write!(f, "Z"),
            Self::Zp => write!(f, "Z'"),
            Self::Z2 => write!(f, "Z2"),
        }
    }
}

impl Move {
    pub fn inverse(self) -> Self {
        match self {
            Self::F  => Self::Fp,
            Self::Fp => Self::F,
            Self::F2 => Self::F2,
            Self::B  => Self::Bp,
            Self::Bp => Self::B,
            Self::B2 => Self::B2,
            Self::L  => Self::Lp,
            Self::Lp => Self::L,
            Self::L2 => Self::L2,
            Self::R  => Self::Rp,
            Self::Rp => Self::R,
            Self::R2 => Self::R2,
            Self::U  => Self::Up,
            Self::Up => Self::U,
            Self::U2 => Self::U2,
            Self::D  => Self::Dp,
            Self::Dp => Self::D,
            Self::D2 => Self::D2,
            Self::f  => Self::fp,
            Self::fp => Self::f,
            Self::f2 => Self::f2,
            Self::b  => Self::bp,
            Self::bp => Self::b,
            Self::b2 => Self::b2,
            Self::l  => Self::lp,
            Self::lp => Self::l,
            Self::l2 => Self::l2,
            Self::r  => Self::rp,
            Self::rp => Self::r,
            Self::r2 => Self::r2,
            Self::u  => Self::up,
            Self::up => Self::u,
            Self::u2 => Self::u2,
            Self::d  => Self::dp,
            Self::dp => Self::d,
            Self::d2 => Self::d2,
            Self::M  => Self::Mp,
            Self::Mp => Self::M,
            Self::M2 => Self::M2,
            Self::E  => Self::Ep,
            Self::Ep => Self::E,
            Self::E2 => Self::E2,
            Self::S  => Self::Sp,
            Self::Sp => Self::S,
            Self::S2 => Self::S2,
            Self::X  => Self::Xp,
            Self::Xp => Self::X,
            Self::X2 => Self::X2,
            Self::Y  => Self::Yp,
            Self::Yp => Self::Y,
            Self::Y2 => Self::Y2,
            Self::Z  => Self::Zp,
            Self::Zp => Self::Z,
            Self::Z2 => Self::Z2,
        }
    }

    pub fn is_inverse(self, other: Self) -> bool { self.inverse() == other }

    pub fn commutes_with(self, other: Self) -> bool {
        match (self, other) {
            (Self::F,  Self::B ) => true,
            (Self::F,  Self::Bp) => true,
            (Self::F,  Self::B2) => true,
            (Self::Fp, Self::B ) => true,
            (Self::Fp, Self::Bp) => true,
            (Self::Fp, Self::B2) => true,
            (Self::F2, Self::B ) => true,
            (Self::F2, Self::Bp) => true,
            (Self::F2, Self::B2) => true,
            (Self::F,  Self::S ) => true,
            (Self::F,  Self::Sp) => true,
            (Self::F,  Self::S2) => true,
            (Self::Fp, Self::S ) => true,
            (Self::Fp, Self::Sp) => true,
            (Self::Fp, Self::S2) => true,
            (Self::F2, Self::S ) => true,
            (Self::F2, Self::Sp) => true,
            (Self::F2, Self::S2) => true,
            (Self::F,  Self::b ) => true,
            (Self::F,  Self::bp) => true,
            (Self::F,  Self::b2) => true,
            (Self::Fp, Self::b ) => true,
            (Self::Fp, Self::bp) => true,
            (Self::Fp, Self::b2) => true,
            (Self::F2, Self::b ) => true,
            (Self::F2, Self::bp) => true,
            (Self::F2, Self::b2) => true,
            (Self::f,  Self::B ) => true,
            (Self::f,  Self::Bp) => true,
            (Self::f,  Self::B2) => true,
            (Self::fp, Self::B ) => true,
            (Self::fp, Self::Bp) => true,
            (Self::fp, Self::B2) => true,
            (Self::f2, Self::B ) => true,
            (Self::f2, Self::Bp) => true,
            (Self::f2, Self::B2) => true,
            (Self::B,  Self::F ) => true,
            (Self::B,  Self::Fp) => true,
            (Self::B,  Self::F2) => true,
            (Self::Bp, Self::F ) => true,
            (Self::Bp, Self::Fp) => true,
            (Self::Bp, Self::F2) => true,
            (Self::B2, Self::F ) => true,
            (Self::B2, Self::Fp) => true,
            (Self::B2, Self::F2) => true,
            (Self::B,  Self::S ) => true,
            (Self::B,  Self::Sp) => true,
            (Self::B,  Self::S2) => true,
            (Self::Bp, Self::S ) => true,
            (Self::Bp, Self::Sp) => true,
            (Self::Bp, Self::S2) => true,
            (Self::B2, Self::S ) => true,
            (Self::B2, Self::Sp) => true,
            (Self::B2, Self::S2) => true,
            (Self::B,  Self::f ) => true,
            (Self::B,  Self::fp) => true,
            (Self::B,  Self::f2) => true,
            (Self::Bp, Self::f ) => true,
            (Self::Bp, Self::fp) => true,
            (Self::Bp, Self::f2) => true,
            (Self::B2, Self::f ) => true,
            (Self::B2, Self::fp) => true,
            (Self::B2, Self::f2) => true,
            (Self::b,  Self::F ) => true,
            (Self::b,  Self::Fp) => true,
            (Self::b,  Self::F2) => true,
            (Self::bp, Self::F ) => true,
            (Self::bp, Self::Fp) => true,
            (Self::bp, Self::F2) => true,
            (Self::b2, Self::F ) => true,
            (Self::b2, Self::Fp) => true,
            (Self::b2, Self::F2) => true,
            (Self::L,  Self::R ) => true,
            (Self::L,  Self::Rp) => true,
            (Self::L,  Self::R2) => true,
            (Self::Lp, Self::R ) => true,
            (Self::Lp, Self::Rp) => true,
            (Self::Lp, Self::R2) => true,
            (Self::L2, Self::R ) => true,
            (Self::L2, Self::Rp) => true,
            (Self::L2, Self::R2) => true,
            (Self::L,  Self::M ) => true,
            (Self::L,  Self::Mp) => true,
            (Self::L,  Self::M2) => true,
            (Self::Lp, Self::M ) => true,
            (Self::Lp, Self::Mp) => true,
            (Self::Lp, Self::M2) => true,
            (Self::L2, Self::M ) => true,
            (Self::L2, Self::Mp) => true,
            (Self::L2, Self::M2) => true,
            (Self::L,  Self::r ) => true,
            (Self::L,  Self::rp) => true,
            (Self::L,  Self::r2) => true,
            (Self::Lp, Self::r ) => true,
            (Self::Lp, Self::rp) => true,
            (Self::Lp, Self::r2) => true,
            (Self::L2, Self::r ) => true,
            (Self::L2, Self::rp) => true,
            (Self::L2, Self::r2) => true,
            (Self::l,  Self::R ) => true,
            (Self::l,  Self::Rp) => true,
            (Self::l,  Self::R2) => true,
            (Self::lp, Self::R ) => true,
            (Self::lp, Self::Rp) => true,
            (Self::lp, Self::R2) => true,
            (Self::l2, Self::R ) => true,
            (Self::l2, Self::Rp) => true,
            (Self::l2, Self::R2) => true,
            (Self::R,  Self::L ) => true,
            (Self::R,  Self::Lp) => true,
            (Self::R,  Self::L2) => true,
            (Self::Rp, Self::L ) => true,
            (Self::Rp, Self::Lp) => true,
            (Self::Rp, Self::L2) => true,
            (Self::R2, Self::L ) => true,
            (Self::R2, Self::Lp) => true,
            (Self::R2, Self::L2) => true,
            (Self::R,  Self::M ) => true,
            (Self::R,  Self::Mp) => true,
            (Self::R,  Self::M2) => true,
            (Self::Rp, Self::M ) => true,
            (Self::Rp, Self::Mp) => true,
            (Self::Rp, Self::M2) => true,
            (Self::R2, Self::M ) => true,
            (Self::R2, Self::Mp) => true,
            (Self::R2, Self::M2) => true,
            (Self::R,  Self::l ) => true,
            (Self::R,  Self::lp) => true,
            (Self::R,  Self::l2) => true,
            (Self::Rp, Self::l ) => true,
            (Self::Rp, Self::lp) => true,
            (Self::Rp, Self::l2) => true,
            (Self::R2, Self::l ) => true,
            (Self::R2, Self::lp) => true,
            (Self::R2, Self::l2) => true,
            (Self::r,  Self::L ) => true,
            (Self::r,  Self::Lp) => true,
            (Self::r,  Self::L2) => true,
            (Self::rp, Self::L ) => true,
            (Self::rp, Self::Lp) => true,
            (Self::rp, Self::L2) => true,
            (Self::r2, Self::L ) => true,
            (Self::r2, Self::Lp) => true,
            (Self::r2, Self::L2) => true,
            (Self::U,  Self::D ) => true,
            (Self::U,  Self::Dp) => true,
            (Self::U,  Self::D2) => true,
            (Self::Up, Self::D ) => true,
            (Self::Up, Self::Dp) => true,
            (Self::Up, Self::D2) => true,
            (Self::U2, Self::D ) => true,
            (Self::U2, Self::Dp) => true,
            (Self::U2, Self::D2) => true,
            (Self::U,  Self::E ) => true,
            (Self::U,  Self::Ep) => true,
            (Self::U,  Self::E2) => true,
            (Self::Up, Self::E ) => true,
            (Self::Up, Self::Ep) => true,
            (Self::Up, Self::E2) => true,
            (Self::U2, Self::E ) => true,
            (Self::U2, Self::Ep) => true,
            (Self::U2, Self::E2) => true,
            (Self::U,  Self::d ) => true,
            (Self::U,  Self::dp) => true,
            (Self::U,  Self::d2) => true,
            (Self::Up, Self::d ) => true,
            (Self::Up, Self::dp) => true,
            (Self::Up, Self::d2) => true,
            (Self::U2, Self::d ) => true,
            (Self::U2, Self::dp) => true,
            (Self::U2, Self::d2) => true,
            (Self::u,  Self::D ) => true,
            (Self::u,  Self::Dp) => true,
            (Self::u,  Self::D2) => true,
            (Self::up, Self::D ) => true,
            (Self::up, Self::Dp) => true,
            (Self::up, Self::D2) => true,
            (Self::u2, Self::D ) => true,
            (Self::u2, Self::Dp) => true,
            (Self::u2, Self::D2) => true,
            (Self::D,  Self::U ) => true,
            (Self::D,  Self::Up) => true,
            (Self::D,  Self::U2) => true,
            (Self::Dp, Self::U ) => true,
            (Self::Dp, Self::Up) => true,
            (Self::Dp, Self::U2) => true,
            (Self::D2, Self::U ) => true,
            (Self::D2, Self::Up) => true,
            (Self::D2, Self::U2) => true,
            (Self::D,  Self::E ) => true,
            (Self::D,  Self::Ep) => true,
            (Self::D,  Self::E2) => true,
            (Self::Dp, Self::E ) => true,
            (Self::Dp, Self::Ep) => true,
            (Self::Dp, Self::E2) => true,
            (Self::D2, Self::E ) => true,
            (Self::D2, Self::Ep) => true,
            (Self::D2, Self::E2) => true,
            (Self::D,  Self::u ) => true,
            (Self::D,  Self::up) => true,
            (Self::D,  Self::u2) => true,
            (Self::Dp, Self::u ) => true,
            (Self::Dp, Self::up) => true,
            (Self::Dp, Self::u2) => true,
            (Self::D2, Self::u ) => true,
            (Self::D2, Self::up) => true,
            (Self::D2, Self::u2) => true,
            (Self::d,  Self::U ) => true,
            (Self::d,  Self::Up) => true,
            (Self::d,  Self::U2) => true,
            (Self::dp, Self::U ) => true,
            (Self::dp, Self::Up) => true,
            (Self::dp, Self::U2) => true,
            (Self::d2, Self::U ) => true,
            (Self::d2, Self::Up) => true,
            (Self::d2, Self::U2) => true,
            (m1, m2) => m1.is_inverse(m2),
        }
    }

    pub fn perms(self) -> Vec<Vec<usize>> {
        match self {
            Self::F  => vec![ vec![ 0,  2,  8,  6]
                            , vec![ 1,  5,  7,  3]
                            , vec![29, 44, 15, 45]
                            , vec![32, 43, 12, 46]
                            , vec![35, 42,  9, 47] ],
            Self::Fp => vec![ vec![ 0,  6,  8,  2]
                            , vec![ 1,  3,  7,  5]
                            , vec![29, 45, 15, 44]
                            , vec![32, 46, 12, 43]
                            , vec![35, 47,  9, 42] ],
            Self::F2 => vec![ vec![ 0,  8], vec![ 2,  6]
                            , vec![ 1,  7], vec![ 5,  3]
                            , vec![29, 15], vec![44, 45]
                            , vec![32, 12], vec![43, 46]
                            , vec![35,  9], vec![42, 47] ],
            Self::B  => vec![ vec![18, 20, 26, 24]
                            , vec![19, 23, 25, 21]
                            , vec![11, 36, 33, 53]
                            , vec![14, 37, 30, 52]
                            , vec![17, 38, 27, 51] ],
            Self::Bp => vec![ vec![18, 24, 26, 20]
                            , vec![19, 21, 25, 23]
                            , vec![11, 53, 33, 36]
                            , vec![14, 52, 30, 37]
                            , vec![17, 51, 27, 38] ],
            Self::B2 => vec![ vec![18, 26], vec![20, 24]
                            , vec![19, 25], vec![23, 21]
                            , vec![11, 33], vec![36, 53]
                            , vec![14, 30], vec![37, 52]
                            , vec![17, 27], vec![38, 51] ],
            Self::L  => vec![ vec![27, 29, 35, 33]
                            , vec![28, 32, 34, 30]
                            , vec![20, 42,  6, 51]
                            , vec![23, 39,  3, 48]
                            , vec![26, 36,  0, 45] ],
            Self::Lp => vec![ vec![27, 33, 35, 29]
                            , vec![28, 30, 34, 32]
                            , vec![20, 51,  6, 42]
                            , vec![23, 48,  3, 39]
                            , vec![26, 45,  0, 36] ],
            Self::L2 => vec![ vec![27, 35], vec![29, 33]
                            , vec![28, 34], vec![32, 30]
                            , vec![20,  6], vec![42, 51]
                            , vec![23,  3], vec![39, 48]
                            , vec![26,  0], vec![36, 45] ],
            Self::R  => vec![ vec![ 9, 11, 17, 15]
                            , vec![10, 14, 16, 12]
                            , vec![ 2, 38, 24, 47]
                            , vec![ 5, 41, 21, 50]
                            , vec![ 8, 44, 18, 53] ],
            Self::Rp => vec![ vec![ 9, 15, 17, 11]
                            , vec![10, 12, 16, 14]
                            , vec![ 2, 47, 24, 38]
                            , vec![ 5, 50, 21, 41]
                            , vec![ 8, 53, 18, 44] ],
            Self::R2 => vec![ vec![ 9, 17], vec![11, 15]
                            , vec![10, 16], vec![14, 12]
                            , vec![ 2, 24], vec![38, 47]
                            , vec![ 5, 21], vec![41, 50]
                            , vec![ 8, 18], vec![44, 53] ],
            Self::U  => vec![ vec![36, 38, 44, 42]
                            , vec![37, 41, 43, 39]
                            , vec![27, 18,  9,  0]
                            , vec![28, 19, 10,  1]
                            , vec![29, 20, 11,  2] ],
            Self::Up => vec![ vec![36, 42, 44, 38]
                            , vec![37, 39, 43, 41]
                            , vec![27,  0,  9, 18]
                            , vec![28,  1, 10, 19]
                            , vec![29,  2, 11, 20] ],
            Self::U2 => vec![ vec![36, 44], vec![38, 42]
                            , vec![37, 43], vec![41, 39]
                            , vec![27,  9], vec![18,  0]
                            , vec![28, 10], vec![19,  1]
                            , vec![29, 11], vec![20,  2] ],
            Self::D  => vec![ vec![45, 47, 53, 51]
                            , vec![46, 50, 52, 48]
                            , vec![35,  8, 17, 26]
                            , vec![34,  7, 16, 25]
                            , vec![33,  6, 15, 24] ],
            Self::Dp => vec![ vec![45, 51, 53, 47]
                            , vec![46, 48, 52, 50]
                            , vec![35, 26, 17,  8]
                            , vec![34, 25, 16,  7]
                            , vec![33, 24, 15,  6] ],
            Self::D2 => vec![ vec![45, 53], vec![47, 51]
                            , vec![46, 52], vec![50, 48]
                            , vec![35, 17], vec![ 8, 26]
                            , vec![34, 16], vec![ 7, 25]
                            , vec![33, 15], vec![ 6, 24] ],
            Self::f  => [Self::F.perms(),  Self::S.perms() ].concat(),
            Self::fp => [Self::Fp.perms(), Self::Sp.perms()].concat(),
            Self::f2 => [Self::F2.perms(), Self::S2.perms()].concat(),
            Self::b  => [Self::B.perms(),  Self::Sp.perms()].concat(),
            Self::bp => [Self::Bp.perms(), Self::S.perms()].concat(),
            Self::b2 => [Self::B2.perms(), Self::S2.perms()].concat(),
            Self::l  => [Self::L.perms(),  Self::M.perms() ].concat(),
            Self::lp => [Self::Lp.perms(), Self::Mp.perms()].concat(),
            Self::l2 => [Self::L2.perms(), Self::M2.perms()].concat(),
            Self::r  => [Self::R.perms(),  Self::Mp.perms()].concat(),
            Self::rp => [Self::Rp.perms(), Self::M.perms() ].concat(),
            Self::r2 => [Self::R2.perms(), Self::M2.perms()].concat(),
            Self::u  => [Self::U.perms(),  Self::Ep.perms()].concat(),
            Self::up => [Self::Up.perms(), Self::E.perms() ].concat(),
            Self::u2 => [Self::U2.perms(), Self::E2.perms()].concat(),
            Self::d  => [Self::D.perms(),  Self::E.perms() ].concat(),
            Self::dp => [Self::Dp.perms(), Self::Ep.perms()].concat(),
            Self::d2 => [Self::D2.perms(), Self::E2.perms()].concat(),
            Self::M  => vec![ vec![ 1, 46, 25, 37]
                            , vec![ 4, 49, 22, 40]
                            , vec![ 7, 52, 19, 43] ],
            Self::Mp => vec![ vec![ 1, 37, 25, 46]
                            , vec![ 4, 40, 22, 49]
                            , vec![ 7, 43, 19, 52] ],
            Self::M2 => vec![ vec![ 1, 25], vec![46, 37]
                            , vec![ 4, 22], vec![49, 40]
                            , vec![ 7, 19], vec![43, 52] ],
            Self::E  => vec![ vec![ 3, 12, 21, 30]
                            , vec![ 4, 13, 22, 31]
                            , vec![ 5, 14, 23, 32] ],
            Self::Ep => vec![ vec![ 3, 30, 21, 12]
                            , vec![ 4, 31, 22, 13]
                            , vec![ 5, 32, 23, 14] ],
            Self::E2 => vec![ vec![ 3, 21], vec![30, 12]
                            , vec![ 4, 22], vec![31, 13]
                            , vec![ 5, 23], vec![32, 14] ],
            Self::S  => vec![ vec![39, 10, 50, 34]
                            , vec![40, 13, 49, 31]
                            , vec![41, 16, 48, 28] ],
            Self::Sp => vec![ vec![39, 34, 50, 10]
                            , vec![40, 31, 49, 13]
                            , vec![41, 28, 48, 16] ],
            Self::S2 => vec![ vec![39, 50], vec![34, 10]
                            , vec![40, 49], vec![31, 13]
                            , vec![41, 48], vec![28, 16] ],
            Self::X  => [Self::Rp.perms(), Self::M.perms(),  Self::L.perms() ].concat(),
            Self::Xp => [Self::R.perms(),  Self::Mp.perms(), Self::Lp.perms()].concat(),
            Self::X2 => [Self::R2.perms(), Self::M2.perms(), Self::L2.perms()].concat(),
            Self::Y  => [Self::U.perms(),  Self::Ep.perms(), Self::Dp.perms()].concat(),
            Self::Yp => [Self::Up.perms(), Self::E.perms(),  Self::D.perms() ].concat(),
            Self::Y2 => [Self::U2.perms(), Self::E2.perms(), Self::D2.perms()].concat(),
            Self::Z  => [Self::F.perms(),  Self::S.perms(),  Self::Bp.perms()].concat(),
            Self::Zp => [Self::Fp.perms(), Self::Sp.perms(), Self::B.perms() ].concat(),
            Self::Z2 => [Self::F2.perms(), Self::S2.perms(), Self::B2.perms()].concat(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Seq(Vec<Move>);

impl Seq {
    pub fn inverse(&self) -> Self {
        let mut new = self.clone();
        new.0.reverse();
        new.0.iter_mut().for_each(|m| { *m = m.inverse(); });
        new
    }

    #[allow(unused_variables)]
    pub fn compose(&self, b: &Self) -> Self {
        todo!()
    }

    pub fn commutator(&self, b: &Self) -> Self {
        let ap = self.inverse();
        let bp = b.inverse();
        self.compose(b).compose(&ap).compose(&bp)
    }

    pub fn conjugate(&self, b: &Self) -> Self {
        let ap = self.inverse();
        self.compose(b).compose(&ap)
    }
}

