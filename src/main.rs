mod model {
    use std::fmt;
    use std::ops::Add;

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Chess {
        Pawn,
        Drone,
        Queen,
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Player {
        Player1,
        Player2,
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    pub struct Playfield {
        board : [[Option<Chess>; 4]; 8],
        scores : [i32; 2],
        turn : Player,
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    struct Point(i32, i32);

    impl Add<Point> for Point {
        type Output = Point;

        fn add(self, other: Point) -> Point {
            Point(self.0 + other.0, self.1 + other.1)
        }
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Action {
        Move {
            start : Point,
            goal : Point,
        },
        Capture {
            start : Point,
            goal : Point,
        },
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    struct OutOfBounds(Point);

    impl Chess {
        fn point(&self) -> i32 {
            match self {
                Chess::Pawn => 1,
                Chess::Drone => 2,
                Chess::Queen => 3,
            }
        }
    }

    impl Player {
        fn index(&self) -> i32 {
            match self {
                Player::Player1 => 0,
                Player::Player2 => 1,
            }
        }

        fn next(&self) -> Player {
            match self {
                Player::Player1 => Player::Player2,
                Player::Player2 => Player::Player1,
            }
        }
    }

    impl Playfield {
        pub fn init() -> Playfield {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            Playfield {
                board : [
                    [q, q, d, n],
                    [q, d, p, n],
                    [d, p, p, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, p, d],
                    [n, p, d, q],
                    [n, d, q, q],
                ],
                scores : [0, 0],
                turn : Player::Player1,
            }
        }

        pub fn get(&self, point: Point) -> Result<(Player, Option<Chess>), OutOfBounds> {
            if point.0 >= 0 && point.0 < 4 && point.1 >= 0 && point.1 < 4 {
                Ok((Player::Player1, self.board[point.0 as usize][point.1 as usize]))
            } else if point.0 >= 4 && point.0 < 8 && point.1 >= 0 && point.1 < 4 {
                Ok((Player::Player2, self.board[point.0 as usize][point.1 as usize]))
            } else {
                Err(OutOfBounds(point))
            }
        }

        pub fn get_chess(&self, point: Point) -> Result<Option<Chess>, OutOfBounds> {
            if !self.is_in_board(point) {
                Err(OutOfBounds)
            } else {
                Ok(self.board[point.0 as usize][point.1 as usize])
            }
        }

    }

    impl fmt::Display for Playfield {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            for line in self.board.iter() {
                for grid in line.iter() {
                    match grid {
                        Option::None => write!(f, ".")?,
                        Option::Some(Chess::Pawn) => write!(f, "p")?,
                        Option::Some(Chess::Drone) => write!(f, "d")?,
                        Option::Some(Chess::Queen) => write!(f, "q")?,
                    }
                    write!(f, " ")?;
                }
                write!(f, "\n")?;
            }
            Ok(())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn point_add() {
            assert_eq!(Point(1, 2) + Point(3, 4), Point(4, 6));
            assert_eq!(Point(1, -2) + Point(-3, 4), Point(-2, 2));
        }

        #[test]
        fn playfield_get_owner() {
            let playfield = Playfield::init();

            for x in 0..4 {
                for y in 0..4 {
                    assert!(
                        match playfield.get(Point(x, y)) {
                            Ok((Player::Player1, _)) => true,
                            _ => false,
                        }
                    );
                }
            }

            for x in 4..8 {
                for y in 0..4 {
                    assert!(
                        match playfield.get(Point(x, y)) {
                            Ok((Player::Player2, _)) => true,
                            _ => false,
                        }
                    );
                }
            }
        }

        #[test]
        fn playfield_get_out_of_bounds() {
            let playfield = Playfield::init();

            assert_eq!(playfield.get(Point(-1, 3)), Err(OutOfBounds(Point(-1, 3))));
            assert_eq!(playfield.get(Point(9, 3)), Err(OutOfBounds(Point(9, 3))));
            assert_eq!(playfield.get(Point(2, -2)), Err(OutOfBounds(Point(2, -2))));
            assert_eq!(playfield.get(Point(2, 4)), Err(OutOfBounds(Point(2, 4))));
        }
    }
}

fn main() {
    let playfield = model::Playfield::init();
    println!("{}", playfield);
}
